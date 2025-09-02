package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;

public class IRForward extends IRInstruction {
  private IRSessionId source;
  private IRSessionId target;
  private IRDataLocation sourceData;
  private IRDataLocation targetData;
  private IRSlotCombinations slots;

  public IRForward(IRSessionId source, IRSessionId target, IRDataLocation sourceData, IRDataLocation targetData, IRSlotCombinations slots) {
    this.source = source;
    this.target = target;
    this.sourceData = sourceData;
    this.targetData = targetData;
    this.slots = slots;
  }

  public IRSessionId getSource() {
    return source;
  }

  public IRSessionId getTarget() {
    return target;
  }

  public IRDataLocation getSourceData() {
    return sourceData;
  }

  public IRDataLocation getTargetData() {
    return targetData;
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
    return new IRForward(source, target, sourceData, targetData, slots);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    sourceData = replacer.apply(sourceData);
    targetData = replacer.apply(targetData);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    source = replacer.apply(source);
    target = replacer.apply(target);
  }

  @Override
  public String toString() {
    return "forward(" + target + ", " + targetData + " <- " + source + ", " + sourceData + ", " + slots + ")";
  }
}
