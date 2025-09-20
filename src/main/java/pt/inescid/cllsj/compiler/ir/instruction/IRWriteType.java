package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRWriteType extends IRWrite {
  private IRSlotTree slots;
  private IRTypeFlagRequisites valueRequisites;

  public IRWriteType(
      IRDataLocation location, IRSlotTree typeSlots, IRTypeFlagRequisites valueRequisites) {
    super(location);
    this.slots = typeSlots;
    this.valueRequisites = valueRequisites;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  public IRTypeFlagRequisites getValueRequisites() {
    return valueRequisites;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteType(location, slots, valueRequisites);
  }

  @Override
  public String toString() {
    return "writeType("
        + location.toString()
        + ", "
        + slots.toString()
        + ", value="
        + valueRequisites
        + ")";
  }
}
