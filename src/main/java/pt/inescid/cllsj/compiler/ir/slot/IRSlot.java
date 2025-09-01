package pt.inescid.cllsj.compiler.ir.slot;

public abstract class IRSlot {
  public abstract void accept(IRSlotVisitor visitor);
}
