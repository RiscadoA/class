package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRFinishSession extends IRInstruction {
  private IRSessionId sessionId;
  private boolean isEndPoint;

  public IRFinishSession(IRSessionId sessionId, boolean isEndPoint) {
    this.sessionId = sessionId;
    this.isEndPoint = isEndPoint;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public boolean isEndPoint() {
    return isEndPoint;
  }

  public void removeEndPoint() {
    isEndPoint = false;
  }

  @Override
  public IRInstruction clone() {
    return new IRFinishSession(sessionId, isEndPoint);
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
    StringBuilder sb = new StringBuilder("finishSession(");
    sb.append(sessionId);
    if (isEndPoint) {
      sb.append(", end point");
    }
    sb.append(")");
    return sb.toString();
  }
}
