package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRTieSessions extends IRInstruction {
  private IRSessionId lhsId;
  private IRSessionId rhsId;

  public IRTieSessions(
      IRSessionId lhsId, IRSessionId rhsId
  ) {
    this.lhsId = lhsId;
    this.rhsId = rhsId;
  }

  public IRSessionId getLhsId() {
    return lhsId;
  }

  public IRSessionId getRhsId() {
    return rhsId;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRTieSessions(lhsId, rhsId);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    lhsId = replacer.apply(lhsId);
    rhsId = replacer.apply(rhsId);
  }

  @Override
  public String toString() {
    return "tieSessions("
        + lhsId
        + ", "
        + rhsId
        + ")";
  }
}
