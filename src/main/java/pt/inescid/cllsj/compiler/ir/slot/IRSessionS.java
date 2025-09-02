package pt.inescid.cllsj.compiler.ir.slot;

public class IRSessionS extends IRSlot {
  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "session";
  }
}
