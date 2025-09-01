package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public class IRWriteScan extends IRWrite {
  private IRSlot slot;

  public IRWriteScan(IRDataLocation location, IRSlot slot) {
    super(location);
    this.slot = slot;
  }

  public IRSlot getSlot() {
    return slot;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteScan(location, slot);
  }

  @Override
  public String toString() {
    return "writeScan(" + location + ", " + slot + ")";
  }
}
