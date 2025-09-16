package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRInitializeSession extends IRInstruction {
  private IRSessionId sessionId;
  private Optional<IRCodeLocation> continuation;
  private IRDataLocation continuationData;

  public IRInitializeSession(
      IRSessionId sessionId,
      Optional<IRCodeLocation> continuation,
      IRDataLocation continuationData) {
    this.sessionId = sessionId;
    this.continuation = continuation;
    this.continuationData = continuationData;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public Optional<IRCodeLocation> getContinuation() {
    return continuation;
  }

  public IRDataLocation getContinuationData() {
    return continuationData;
  }

  @Override
  public IRInstruction clone() {
    return new IRInitializeSession(sessionId, continuation, continuationData);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    sessionId = replacer.apply(sessionId);
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    super.replaceCodeLocations(replacer);
    continuation = continuation.map(replacer);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    continuationData = replacer.apply(continuationData);
  }

  public void removeContinuation() {
    continuation = Optional.empty();
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("initializeSession(");
    sb.append(sessionId).append(", ");
    if (continuation.isPresent()) {
      sb.append(continuation.get()).append(", ");
    }
    sb.append(continuationData).append(")");
    return sb.toString();
  }
}
