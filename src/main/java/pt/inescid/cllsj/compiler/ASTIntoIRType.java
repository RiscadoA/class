package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.List;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCaseT;
import pt.inescid.cllsj.ast.types.ASTCoLboolT;
import pt.inescid.cllsj.ast.types.ASTCoLstringT;
import pt.inescid.cllsj.ast.types.ASTCoRecT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTLCointT;
import pt.inescid.cllsj.ast.types.ASTLboolT;
import pt.inescid.cllsj.ast.types.ASTLintT;
import pt.inescid.cllsj.ast.types.ASTLstringT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTOfferT;
import pt.inescid.cllsj.ast.types.ASTOneT;
import pt.inescid.cllsj.ast.types.ASTRecT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTRecvTT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTSendTT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTWhyT;
import pt.inescid.cllsj.ast.types.ASTintT;
import pt.inescid.cllsj.compiler.ir.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.type.IRCloseT;
import pt.inescid.cllsj.compiler.ir.type.IRExponentialT;
import pt.inescid.cllsj.compiler.ir.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.type.IRRecT;
import pt.inescid.cllsj.compiler.ir.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.type.IRTagT;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRTypeT;
import pt.inescid.cllsj.compiler.ir.type.IRVarT;

public class ASTIntoIRType extends ASTTypeVisitor {
  private Env<EnvEntry> ep;
  private IRType ir;

  public static IRType convert(Env<EnvEntry> ep, ASTType type) {
    ASTIntoIRType converter = new ASTIntoIRType(ep);
    type.accept(converter);
    return converter.ir;
  }

  private ASTIntoIRType(Env<EnvEntry> ep) {
    this.ep = ep;
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
    ir =
        new IRSessionT(
            ASTIntoIRType.convert(ep, type.getlhs()), ASTIntoIRType.convert(ep, type.getrhs()));
  }

  @Override
  public void visit(ASTRecvT type) {
    ir =
        new IRSessionT(
            ASTIntoIRType.convert(ep, type.getlhs()), ASTIntoIRType.convert(ep, type.getrhs()));
  }

  @Override
  public void visit(ASTCaseT type) {
    List<IRType> choices = new ArrayList<>();
    for (int i = 0; i < type.getcases().size(); i++) {
      choices.add(ASTIntoIRType.convert(ep, type.getCaseType(type.getLabel(i))));
    }
    ir = new IRTagT(choices);
  }

  @Override
  public void visit(ASTOfferT type) {
    List<IRType> choices = new ArrayList<>();
    for (int i = 0; i < type.getcases().size(); i++) {
      choices.add(ASTIntoIRType.convert(ep, type.getCaseType(type.getLabel(i))));
    }
    ir = new IRTagT(choices);
  }

  @Override
  public void visit(ASTRecT type) {
    ir =
        new IRRecT(
            ASTIntoIRType.convert(
                ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getin()));
  }

  @Override
  public void visit(ASTCoRecT type) {
    ir =
        new IRRecT(
            ASTIntoIRType.convert(
                ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getin()));
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
      ir = new IRVarT();
    } else {
      unfolded.accept(this);
    }
  }

  @Override
  public void visit(ASTBangT type) {
    ir = new IRExponentialT(ASTIntoIRType.convert(ep, type.getin()));
  }

  @Override
  public void visit(ASTWhyT type) {
    ir = new IRExponentialT(ASTIntoIRType.convert(ep, type.getin()));
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
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTSendTT type) {
    ir = new IRTypeT(ASTIntoIRType.convert(
      ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))),
        type.getrhs()));
  }

  @Override
  public void visit(ASTRecvTT type) {
    ir =
        new IRTypeT(
            ASTIntoIRType.convert(
              ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))), type.getrhs()));
  }
}
