package pt.inescid.cllsj.compiler.ir.slot;

import pt.inescid.cllsj.compiler.ir.IRSlotVisitor;

public abstract class IRSlot {
  public abstract void accept(IRSlotVisitor visitor);
}
