package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRDropSlots extends IRInstruction {
  private IRDataLocation location;
  private IRSlotTree slots;

  public IRDropSlots(IRDataLocation location, IRSlotTree slots) {
    this.location = location;
    this.slots = slots;
  }

  public IRDataLocation getLocation() {
    return location;
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
    return new IRDropSlots(location, slots);
  }

  @Override
  public String toString() {
    return "dropSlots(" + location + ", " + slots + ")";
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    location = replacer.apply(location);
  }

  @Override
  public void replaceSlots(Function<IRSlotTree, IRSlotTree> replacer) {
    super.replaceSlots(replacer);
    slots = replacer.apply(slots);
  }
}
