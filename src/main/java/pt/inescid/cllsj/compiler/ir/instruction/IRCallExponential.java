package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRCallExponential extends IRAccess {
  private IRSessionId sessionId;
  private IRLocalDataId localDataId;
  private boolean decrementExponential;

  public IRCallExponential(
      IRDataLocation location,
      IRSessionId sessionId,
      IRLocalDataId localDataId,
      boolean decrementExponential) {
    super(location);
    this.sessionId = sessionId;
    this.localDataId = localDataId;
    this.decrementExponential = decrementExponential;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public IRLocalDataId getLocalDataId() {
    return localDataId;
  }

  public boolean shouldDecrementExponential() {
    return decrementExponential;
  }

  @Override
  public IRInstruction clone() {
    return new IRCallExponential(location, sessionId, localDataId, decrementExponential);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("callExponential(")
        .append(location)
        .append(", ")
        .append(sessionId)
        .append(", ")
        .append(localDataId);
    if (decrementExponential) {
      sb.append(", decrement");
    }
    sb.append(")");
    return sb.toString();
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    sessionId = replacer.apply(sessionId);
  }
}
