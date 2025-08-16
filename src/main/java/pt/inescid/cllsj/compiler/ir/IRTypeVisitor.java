package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.type.*;
import pt.inescid.cllsj.compiler.ir.type.branch.IRPolarityBranchT;
import pt.inescid.cllsj.compiler.ir.type.branch.IRTagT;
import pt.inescid.cllsj.compiler.ir.type.branch.IRValueBranchT;
import pt.inescid.cllsj.compiler.ir.type.slot.*;

public abstract class IRTypeVisitor {
  // Catch all for types which do not have their own visit method
  public abstract void visit(IRType type);

  public void visit(IRSlotT type) {
    visit((IRType) type);
  }

  public void visit(IRCloseT type) {
    visit((IRType) type);
  }

  public void visit(IRBranchT type) {
    visit((IRType) type);
  }

  public void visit(IRFlipT type) {
    visit((IRType) type);
  }

  public void visit(IRRecT type) {
    visit((IRType) type);
  }

  public void visit(IRVarT type) {
    visit((IRType) type);
  }

  public void visit(IRSessionT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRExponentialT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRIntT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRBoolT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRStringT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRTypeT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRCellT type) {
    visit((IRSlotT) type);
  }

  public void visit(IRTagT type) {
    visit((IRBranchT) type);
  }

  public void visit(IRPolarityBranchT type) {
    visit((IRBranchT) type);
  }

  public void visit(IRValueBranchT type) {
    visit((IRBranchT) type);
  }
}
