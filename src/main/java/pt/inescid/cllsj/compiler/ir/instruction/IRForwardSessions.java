package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRForwardSessions extends IRInstruction {
  private IRSessionId negId;
  private IRSessionId posId;
  private boolean isEndPoint;
  private boolean shouldJump;

  public IRForwardSessions(
      IRSessionId negId, IRSessionId posId, boolean isEndPoint, boolean shouldJump) {
    this.negId = negId;
    this.posId = posId;
    this.isEndPoint = isEndPoint;
    this.shouldJump = shouldJump;
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

  public boolean shouldJump() {
    return shouldJump;
  }

  public void removeEndPoint() {
    isEndPoint = false;
  }

  public void removeJump() {
    shouldJump = false;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRForwardSessions(negId, posId, isEndPoint, shouldJump);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    negId = replacer.apply(negId);
    posId = replacer.apply(posId);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("forwardSessions(");
    sb.append("-").append(negId).append(", +").append(posId);
    if (isEndPoint) {
      sb.append(", end point");
    }
    if (shouldJump) {
      sb.append(", jump");
    }
    sb.append(")");
    return sb.toString();
  }
}
