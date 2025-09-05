package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRForwardSessions extends IRInstruction {
  private IRSessionId negId;
  private IRSessionId posId;
  private boolean isEndPoint;

  public IRForwardSessions(IRSessionId negId, IRSessionId posId, boolean isEndPoint) {
    this.negId = negId;
    this.posId = posId;
    this.isEndPoint = isEndPoint;
  }

  public IRSessionId getNegId() {
    return negId;
  }

  public IRSessionId getPosId() {
    return posId;
  }

  public boolean isEndPoint() {
    return isEndPoint;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRForwardSessions(negId, posId, isEndPoint);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    negId = replacer.apply(negId);
    posId = replacer.apply(posId);
  }

  @Override
  public String toString() {
    return "forwardSessions(-" + negId + ", +" + posId + (isEndPoint ? ", end point)" : ")");
  }
}
