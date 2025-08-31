package pt.inescid.cllsj.compiler.ir.slot;

import pt.inescid.cllsj.compiler.ir.IRSlotVisitor;

public class IRBoolS extends IRSlot {
  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "bool";
  }
}
