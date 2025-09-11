package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRDropValue extends IRInstruction {
  private IRDataLocation location;
  private IRSlotTree slots;

  public IRDropValue(IRDataLocation location, IRSlotTree slots) {
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
    return new IRDropValue(location, slots);
  }

  @Override
  public String toString() {
    return "dropValue(" + location + ", " + slots + ")";
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    location = replacer.apply(location);
  }
}
