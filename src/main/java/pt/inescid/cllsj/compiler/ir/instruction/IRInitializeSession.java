package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRInitializeSession extends IRInstruction {
  private IRSessionId sessionId;
  private IRCodeLocation continuation;
  private IRDataLocation continuationData;

  public IRInitializeSession(
      IRSessionId sessionId, IRCodeLocation continuation, IRDataLocation continuationData) {
    this.sessionId = sessionId;
    this.continuation = continuation;
    this.continuationData = continuationData;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public IRCodeLocation getContinuation() {
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
    sessionId = replacer.apply(sessionId);
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    continuation = replacer.apply(continuation);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    continuationData = replacer.apply(continuationData);
  }

  @Override
  public String toString() {
    return "initializeSession(" + sessionId + ", " + continuation + ", " + continuationData + ")";
  }
}
