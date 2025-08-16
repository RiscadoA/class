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
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.type.*;
import pt.inescid.cllsj.compiler.ir.type.branch.*;
import pt.inescid.cllsj.compiler.ir.type.slot.*;

public class ASTIntoIRType extends ASTTypeVisitor {
  private IRGenerator gen;
  private Env<EnvEntry> ep;
  private Map<String, Integer> typeMap = new HashMap<>();
  private List<Boolean> typePolarities;
  private IRType ir;
  private IRType closingType;
  private boolean isDual;

  public static IRType convert(
      IRGenerator gen, Env<EnvEntry> ep, ASTType type, Map<String, Integer> typeMap, List<Boolean> typePolarities) {
    return convert(gen, ep, type, typeMap, typePolarities, new IRCloseT(), false);
  }

  public static IRType convert(
      IRGenerator gen,
      Env<EnvEntry> ep,
      ASTType type,
      Map<String, Integer> typeMap,
      List<Boolean> typePolarities,
      IRType closingType,
      boolean isDual) {
    ASTIntoIRType converter = new ASTIntoIRType(ep, typeMap, typePolarities, closingType, isDual);

    if (isPositive(ep, type, typeMap, typePolarities, isDual)) {
      type.accept(converter);
    } else {
      converter.isDual = !converter.isDual;
      type.dualCatch(ep).accept(converter);
    }
        
    return converter.ir;
  }

  private IRType recurse(Env<EnvEntry> ep, ASTType type) {
    return recurse(ep, type, closingType);
  }

  private IRType recurse(Env<EnvEntry> ep, ASTType type, IRType closingType) {
    if (isPositive(ep, type, typeMap, typePolarities, isDual)) {
      return convert(gen, ep, type, typeMap, typePolarities, closingType, isDual);
    } else {
      return new IRFlipT(convert(gen, ep, type, typeMap, typePolarities, closingType, !isDual));
    }
  }

  private ASTIntoIRType(
      Env<EnvEntry> ep,
      Map<String, Integer> typeMap,
      List<Boolean> typePolarities,
      IRType closingType,
      boolean isDual) {
    this.ep = ep;
    this.typeMap = typeMap;
    this.typePolarities = typePolarities;
    this.closingType = closingType;
    this.isDual = isDual;
  }

  private void pushType(String id, boolean isPositive) {
    for (Map.Entry<String, Integer> entry : this.typeMap.entrySet()) {
      entry.setValue(entry.getValue() + 1);
    }
    typeMap.put(id, 0);
    typePolarities.addFirst(isDual ^ isPositive);
  }

  private void popType() {
    Optional<String> toRemove = Optional.empty();
    for (Map.Entry<String, Integer> e : typeMap.entrySet()) {
      if (e.getValue() == 0) {
        toRemove = Optional.of(e.getKey());
      } else {
        e.setValue(e.getValue() - 1);
      }
    }
    typeMap.remove(toRemove.get());
    typePolarities.removeFirst();
  }

  @Override
  public void visit(ASTType type) {
    throw new UnsupportedOperationException("Type not supported by IntoIRType: " + type);
  }

  @Override
  public void visit(ASTNotT type) {
    ir = convert(gen, ep, type.getin(), typeMap, typePolarities, closingType, !isDual);
  }

  @Override
  public void visit(ASTOneT type) {
    ir = closingType;
  }

  @Override
  public void visit(ASTSendT type) {
    IRType cont = recurse(ep, type.getrhs());

    IRValueRequisites requisites = ASTTypeIsValue.check(gen, ep, typeMap, type.getlhs(), false);

    IRType value = recurse(ep, type.getlhs(), cont);
    IRType notValue = new IRSessionT(recurse(ep, type.getlhs(), new IRCloseT()), cont);
    
    if (requisites.mustBeValue()) {
      ir = value;
    } else if (requisites.canBeValue()) {
      ir = new IRValueBranchT(requisites, value, notValue);
    } else {
      ir = notValue;
    }
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
  public void visit(ASTRecT type) {
    pushType(type.getid(), true);
    ir =
        new IRRecT(
            recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getin()));
    popType();
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
        throw new IllegalArgumentException("Type not found in environment: " + type.getid());
      }

      int typeId = typeMap.get(type.getid());
      IRType var = new IRVarT(typeId, closingType);
      IRType positive = isDual ? new IRFlipT(var) : var;
      IRType negative = isDual ? var : new IRFlipT(var);
      ir = new IRPolarityBranchT(typeId, positive, negative);
    } else {
      unfolded.accept(this);
    }
  }

  @Override
  public void visit(ASTBangT type) {
    ir = new IRExponentialT(recurse(ep, type.getin(), new IRCloseT()), closingType);
  }

  @Override
  public void visit(ASTintT type) {
    ir = new IRIntT(closingType);
  }

  @Override
  public void visit(ASTLintT type) {
    ir = new IRIntT(closingType);
  }

  @Override
  public void visit(ASTLboolT type) {
    ir = new IRBoolT(closingType);
  }

  @Override
  public void visit(ASTLstringT type) {
    ir = new IRStringT(closingType);
  }

  @Override
  public void visit(ASTSendTT type) {
    pushType(type.getid(), true);
    IRType positive = recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs(), new IRCloseT());
    popType();
    pushType(type.getid(), false);
    IRType negative = recurse(ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs(), new IRCloseT());
    popType();

    ir = new IRPolarityBranchT(0, positive, negative);
    ir = new IRTypeT(ir, closingType);
  }

  @Override
  public void visit(ASTAffineT type) {
    ir = recurse(ep, type);
  }

  @Override
  public void visit(ASTCellT type) {
    ir = new IRCellT(recurse(ep, type.getin()), closingType);
  }

  @Override
  public void visit(ASTCellLT type) {
    ir = new IRCellT(recurse(ep, type.getin()), closingType);
  }

  private static boolean isPositive(
      Env<EnvEntry> ep,
      ASTType type,
      Map<String, Integer> typeMap,
      List<Boolean> typePolarities,
      boolean isDual) {
    Optional<Boolean> typePolarity = type.getPolarityCatch(ep);
    if (typePolarity.isPresent()) {
      return typePolarity.get();
    }

    // If it is a type variable, find its polarity
    boolean hasNot = false;
    type = type.unfoldTypeCatch(ep);
    if (type instanceof ASTNotT) {
      hasNot = true;
      type = ((ASTNotT)type).getin();
    }
    if (!(type instanceof ASTIdT)) {
      throw new UnsupportedOperationException("Expected a type variable");
    }
    
    int index = typeMap.get(((ASTIdT)type).getid());
    return isDual ^ hasNot ^ typePolarities.get(index);
  }
}
