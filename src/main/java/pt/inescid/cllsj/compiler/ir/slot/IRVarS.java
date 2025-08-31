package pt.inescid.cllsj.compiler.ir.slot;

import pt.inescid.cllsj.compiler.ir.IRSlotVisitor;

public class IRVarS extends IRSlot {
  private int typeId;

  public IRVarS(int typeId) {
    this.typeId = typeId;
  }

  public int getTypeId() {
    return typeId;
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "var(" + typeId + ")";
  }
}
