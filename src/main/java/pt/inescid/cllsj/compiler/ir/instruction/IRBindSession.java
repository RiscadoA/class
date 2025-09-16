package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRBindSession extends IRAccess {
  private IRSessionId sessionId;
  private IRDataLocation continuationData;

  public IRBindSession(
      IRDataLocation location, IRSessionId sessionId, IRDataLocation continuationData) {
    super(location);
    this.sessionId = sessionId;
    this.continuationData = continuationData;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public IRDataLocation getContinuationData() {
    return continuationData;
  }

  @Override
  public IRInstruction clone() {
    return new IRBindSession(location, sessionId, continuationData);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "bindSession(" + location + ", " + sessionId + ", " + continuationData + ")";
  }

  @Override
  public void replaceSessions(java.util.function.Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    sessionId = replacer.apply(sessionId);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    continuationData = replacer.apply(continuationData);
  }
}
