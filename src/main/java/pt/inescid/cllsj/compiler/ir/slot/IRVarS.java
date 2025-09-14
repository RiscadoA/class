package pt.inescid.cllsj.compiler.ir.slot;

import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRVarS extends IRSlot {
  private IRTypeId typeId;

  public IRVarS(IRTypeId typeId) {
    this.typeId = typeId;
  }

  public IRTypeId getTypeId() {
    return typeId;
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return typeId.toString();
  }
}
