package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRCloneValue extends IRWrite {
  private IRDataLocation sourceLocation;
  private IRSlotTree slots;

  public IRCloneValue(
      IRDataLocation targetLocation, IRDataLocation sourceLocation, IRSlotTree slots) {
    super(targetLocation);
    this.sourceLocation = sourceLocation;
    this.slots = slots;
  }

  public IRDataLocation getSourceLocation() {
    return sourceLocation;
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
    return new IRCloneValue(location, sourceLocation, slots);
  }

  @Override
  public String toString() {
    return "cloneValue(" + location + ", " + sourceLocation + ", " + slots + ")";
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    sourceLocation = replacer.apply(sourceLocation);
  }
}
