package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

public class IRWriteValue extends IRWrite {
  private IRDataLocation sourceLocation;
  private IRSlotCombinations slots;

  public IRWriteValue(IRDataLocation location, IRDataLocation sourceLocation, IRSlotCombinations slots) {
    super(location);
    this.sourceLocation = sourceLocation;
    this.slots = slots;
  }

  public IRDataLocation getSourceLocation() {
    return sourceLocation;
  }

  public IRSlotCombinations getSlots() {
    return slots;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteValue(location, sourceLocation, slots);
  }

  @Override
  public String toString() {
    return "writeSession(" + location + ", " + sourceLocation + ", " + slots + ")";
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    sourceLocation = replacer.apply(sourceLocation);
  }
}
