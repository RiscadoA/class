package pt.inescid.cllsj.compiler.ir.slot;

import pt.inescid.cllsj.compiler.ir.IRSlotVisitor;

public class IRSessionS extends IRSlot {
  private IRSlotSequence passedSlots;

  public IRSessionS(IRSlotSequence passedSlots) {
    this.passedSlots = passedSlots;
  }

  public IRSlotSequence getPassedSlots() {
    return passedSlots;
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "session(" + passedSlots.toString() + ")";
  }
}
