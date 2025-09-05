package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRWriteType extends IRWrite {
  private IRSlotTree slots;

  public IRWriteType(IRDataLocation location, IRSlotTree typeSlots) {
    super(location);
    this.slots = typeSlots;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteType(location, slots);
  }

  @Override
  public String toString() {
    return "writeType(" + location.toString() + ", " + slots.toString() + ")";
  }
}
