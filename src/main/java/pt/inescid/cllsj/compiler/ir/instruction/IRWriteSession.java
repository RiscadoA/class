package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRSessionId;

public class IRWriteSession extends IRWrite {
  private IRSessionId sessionId;

  public IRWriteSession(IRDataLocation location, IRSessionId sessionId) {
    super(location);
    this.sessionId = sessionId;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteSession(location, sessionId);
  }

  @Override
  public String toString() {
    return "writeSession(" + location + ", " + sessionId + ")";
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    sessionId = replacer.apply(sessionId);
  }
}
