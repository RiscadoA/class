package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRBindSession extends IRInstruction {
  private IRSessionId targetSessionId;
  private IRDataLocation sourceLocation;
  private IRDataLocation localData;

  public IRBindSession(
      IRSessionId targetSessionId,
      IRDataLocation sourceLocation,
      IRDataLocation localData) {
    this.targetSessionId = targetSessionId;
    this.sourceLocation = sourceLocation;
    this.localData = localData;
  }

  public IRSessionId getTargetSessionId() {
    return targetSessionId;
  }

  public IRDataLocation getSourceLocation() {
    return sourceLocation;
  }

  public IRDataLocation getLocalData() {
    return localData;
  }

  @Override
  public IRInstruction clone() {
    return new IRBindSession(targetSessionId, getSourceLocation(), localData);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("bindSession(");
    sb.append(targetSessionId);
    sb.append(", ");
    sb.append(getSourceLocation());
    sb.append(", ");
    sb.append(localData);
    sb.append(")");
    return sb.toString();
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    targetSessionId = replacer.apply(targetSessionId);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    sourceLocation = replacer.apply(sourceLocation);
    localData = replacer.apply(localData);
  }
}
