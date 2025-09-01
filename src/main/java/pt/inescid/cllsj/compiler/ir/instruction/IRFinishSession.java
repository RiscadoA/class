package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRFinishSession extends IRInstruction {
  private IRSessionId sessionId;

  public IRFinishSession(IRSessionId sessionId) {
    this.sessionId = sessionId;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  @Override
  public IRInstruction clone() {
    return new IRFinishSession(sessionId);
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
  public String toString() {
    return "finishSession(" + sessionId + ")";
  }
}
