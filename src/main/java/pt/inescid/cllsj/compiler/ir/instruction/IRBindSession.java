package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRBindSession extends IRAccess {
  private IRSessionId sessionId;

  public IRBindSession(IRDataLocation location, IRSessionId sessionId) {
    super(location);
    this.sessionId = sessionId;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  @Override
  public IRInstruction clone() {
    return new IRBindSession(location, sessionId);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "bindSession(" + location + ", " + sessionId + ")";
  }

  @Override
  public void replaceSessions(java.util.function.Function<IRSessionId, IRSessionId> replacer) {
    sessionId = replacer.apply(sessionId);
  }
}
