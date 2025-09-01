package pt.inescid.cllsj.compiler.ir.slot;

public class IRTypeS extends IRSlot {
  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "type";
  }
}
