package pt.inescid.cllsj.compiler.old.ir;

import pt.inescid.cllsj.compiler.ir.old.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.old.type.IRCellT;
import pt.inescid.cllsj.compiler.ir.old.type.IRCloseT;
import pt.inescid.cllsj.compiler.ir.old.type.IRExponentialT;
import pt.inescid.cllsj.compiler.ir.old.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.old.type.IRRecT;
import pt.inescid.cllsj.compiler.ir.old.type.IRResetT;
import pt.inescid.cllsj.compiler.ir.old.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.old.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.old.type.IRTagT;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.ir.old.type.IRTypeT;
import pt.inescid.cllsj.compiler.ir.old.type.IRVarT;

public abstract class IRTypeVisitor {
  // Catch all for types which do not have their own visit method
  public abstract void visit(IRType type);

  public void visit(IRCloseT type) {
    visit((IRType) type);
  }

  public void visit(IRTagT type) {
    visit((IRType) type);
  }

  public void visit(IRSessionT type) {
    visit((IRType) type);
  }

  public void visit(IRRecT type) {
    visit((IRType) type);
  }

  public void visit(IRVarT type) {
    visit((IRType) type);
  }

  public void visit(IRExponentialT type) {
    visit((IRType) type);
  }

  public void visit(IRIntT type) {
    visit((IRType) type);
  }

  public void visit(IRBoolT type) {
    visit((IRType) type);
  }

  public void visit(IRStringT type) {
    visit((IRType) type);
  }

  public void visit(IRTypeT type) {
    visit((IRType) type);
  }

  public void visit(IRCellT type) {
    visit((IRType) type);
  }

  public void visit(IRResetT type) {
    visit((IRType) type);
  }
}
