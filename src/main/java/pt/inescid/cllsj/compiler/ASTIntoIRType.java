package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.ir.type.*;

public class ASTIntoIRType extends ASTTypeVisitor {
  private IRGenerator gen;
  private Env<EnvEntry> ep;
  private Map<String, Integer> typeMap = new HashMap<>();
  private IRType ir;
  private Optional<Boolean> lastPolarity;
  private Optional<Boolean> currentPolarity;

  public static IRType convert(
      IRGenerator gen, Env<EnvEntry> ep, ASTType type, Map<String, Integer> typeMap) {
    return convert(gen, ep, type, typeMap, Optional.empty(), type.getPolarityForCompilerCatch(ep));
  }

  public static IRType convert(
      IRGenerator gen,
      Env<EnvEntry> ep,
      ASTType type,
      Map<String, Integer> typeMap,
      Optional<Boolean> lastPolarity,
      Optional<Boolean> currentPolarity) {
    ASTIntoIRType converter = new ASTIntoIRType(gen, ep, typeMap, lastPolarity, currentPolarity);
    type.accept(converter);
    return converter.ir;
  }

  private IRType recurse(Env<EnvEntry> ep, ASTType type) {
    Optional<Boolean> resultPolarity = type.getPolarityForCompilerCatch(ep);
    IRType result = convert(gen, ep, type, typeMap, currentPolarity, resultPolarity);

    if (resultPolarity.isPresent()
        && currentPolarity.isPresent()
        && resultPolarity.get() != currentPolarity.get()) {
      return new IRResetT(result);
    } else {
      return result;
    }
  }

  private ASTIntoIRType(
      IRGenerator gen,
      Env<EnvEntry> ep,
      Map<String, Integer> typeMap,
      Optional<Boolean> lastPolarity,
      Optional<Boolean> currentPolarity) {
    this.gen = gen;
    this.ep = ep;
    this.typeMap = typeMap;
    this.lastPolarity = lastPolarity;
    this.currentPolarity = currentPolarity;
  }

  private Map<String, Integer> insertType(String id) {
    Map<String, Integer> typeMap = new HashMap<>();
    for (Map.Entry<String, Integer> entry : this.typeMap.entrySet()) {
      typeMap.put(entry.getKey(), entry.getValue() + 1);
    }
    typeMap.put(id, 0);
    return typeMap;
  }

  @Override
  public void visit(ASTType type) {
    throw new UnsupportedOperationException("Type not supported by IntoIRType: " + type);
  }

  @Override
  public void visit(ASTOneT type) {
    ir = new IRCloseT();
  }

  @Override
  public void visit(ASTBotT type) {
    ir = new IRCloseT();
  }

  @Override
  public void visit(ASTSendT type) {
    ir = new IRSessionT(recurse(ep, type.getlhs()), recurse(ep, type.getrhs()));
  }

  @Override
  public void visit(ASTRecvT type) {
    ir = new IRSessionT(recurse(ep, type.getlhs()), recurse(ep, type.getrhs()));
  }

  @Override
  public void visit(ASTCaseT type) {
    List<IRType> choices = new ArrayList<>();
    for (int i = 0; i < type.getcases().size(); i++) {
      choices.add(recurse(ep, type.getCaseType(type.getLabel(i))));
    }
    ir = new IRTagT(choices);
  }

  @Override
  public void visit(ASTOfferT type) {
    List<IRType> choices = new ArrayList<>();
    for (int i = 0; i < type.getcases().size(); i++) {
      choices.add(recurse(ep, type.getCaseType(type.getLabel(i))));
    }
    ir = new IRTagT(choices);
  }

  @Override
  public void visit(ASTRecT type) {
    Map<String, Integer> typeMap = this.typeMap;
    this.typeMap = insertType(type.getid());
    ir =
        new IRRecT(
            recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getin()));
    this.typeMap = typeMap;
  }

  @Override
  public void visit(ASTCoRecT type) {
    Map<String, Integer> typeMap = this.typeMap;
    this.typeMap = insertType(type.getid());
    ir =
        new IRRecT(
            recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getin()));
    this.typeMap = typeMap;
  }

  @Override
  public void visit(ASTIdT type) {
    ASTType unfolded;
    try {
      unfolded = type.unfoldType(ep);
    } catch (Exception e) {
      throw new IllegalArgumentException("Error unfolding type: " + e.getMessage());
    }

    if (unfolded instanceof ASTIdT) {
      type = (ASTIdT) unfolded;
      if (!typeMap.containsKey(type.getid())) {
        String knownTypes = "";
        for (String key : typeMap.keySet()) {
          if (!knownTypes.isEmpty()) {
            knownTypes += ", ";
          }
          knownTypes += key;
        }
        throw new IllegalArgumentException(
            "Type not found in environment: " + type.getid() + " (contains {" + knownTypes + "})");
      }
      ir = new IRVarT(typeMap.get(type.getid()), lastPolarity.map(p -> !p));
    } else {
      unfolded.accept(this);
    }
  }

  @Override
  public void visit(ASTBangT type) {
    ir = new IRExponentialT(recurse(ep, type.getin()));
  }

  @Override
  public void visit(ASTWhyT type) {
    ir = new IRExponentialT(recurse(ep, type.getin()));
  }

  @Override
  public void visit(ASTintT type) {
    ir = new IRIntT();
  }

  @Override
  public void visit(ASTCointT type) {
    ir = new IRIntT();
  }

  @Override
  public void visit(ASTLintT type) {
    ir = new IRIntT();
  }

  @Override
  public void visit(ASTLCointT type) {
    ir = new IRIntT();
  }

  @Override
  public void visit(ASTLboolT type) {
    ir = new IRBoolT();
  }

  @Override
  public void visit(ASTCoLboolT type) {
    ir = new IRBoolT();
  }

  @Override
  public void visit(ASTLstringT type) {
    ir = new IRStringT();
  }

  @Override
  public void visit(ASTCoLstringT type) {
    ir = new IRStringT();
  }

  @Override
  public void visit(ASTNotT type) {
    ir =
        convert(
            gen,
            ep,
            type.getin(),
            typeMap,
            lastPolarity.map(p -> !p),
            currentPolarity.map(p -> !p));
  }

  @Override
  public void visit(ASTSendTT type) {
    Map<String, Integer> typeMap = this.typeMap;
    this.typeMap = insertType(type.getid());
    ir =
        new IRTypeT(
            recurse(
                ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs()));
    this.typeMap = typeMap;
  }

  @Override
  public void visit(ASTRecvTT type) {
    Map<String, Integer> typeMap = this.typeMap;
    this.typeMap = insertType(type.getid());
    ir =
        new IRTypeT(
            recurse(
                ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs()));
    this.typeMap = typeMap;
  }

  @Override
  public void visit(ASTAffineT type) {
    ir = new IRTagT(List.of(
      new IRResetT(new IRCloseT()),
      recurse(ep, type.getin())
    ));
  }

  @Override
  public void visit(ASTCoAffineT type) {
    ir = new IRTagT(List.of(
      new IRResetT(new IRCloseT()),
      recurse(ep, type.getin())
    ));
  }

  @Override
  public void visit(ASTCellT type) {
    ir = new IRCellT(recurse(ep, type.getin()));
  }

  @Override
  public void visit(ASTUsageT type) {
    ir = new IRCellT(recurse(ep, type.getin()));
  }

  @Override
  public void visit(ASTCellLT type) {
    ir = new IRCellT(recurse(ep, type.getin()));
  }

  @Override
  public void visit(ASTUsageLT type) {
    ir = new IRCellT(recurse(ep, type.getin()));
  }
}
