package pt.inescid.cllsj.compiler.ir.slot;

// A special type of slot which is identical in layout to the equivalent IRVarS, but with a
// concrete type instead of a type variable. This is used during translation between a
// polymorphic session and its instantiated counterpart.
public class IRKnownVarS extends IRSlot {
  private IRSlotTree slots;

  public IRKnownVarS(IRSlotTree slots) {
    this.slots = slots;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "poly" + slots.toString();
  }
}
