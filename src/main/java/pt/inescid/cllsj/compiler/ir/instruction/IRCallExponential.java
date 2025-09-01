package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRCallExponential extends IRAccess {
  private IRSessionId sessionId;

  public IRCallExponential(IRDataLocation location, IRSessionId sessionId) {
    super(location);
    this.sessionId = sessionId;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  @Override
  public IRInstruction clone() {
    return new IRCallExponential(location, sessionId);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "callExponential(" + location + ", " + sessionId + ")";
  }

  @Override
  public void replaceSessions(java.util.function.Function<IRSessionId, IRSessionId> replacer) {
    sessionId = replacer.apply(sessionId);
  }
}
