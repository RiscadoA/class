package pt.inescid.cllsj.compiler.ir.slot;

public class IRIntS extends IRSlot {
  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "int";
  }
}
