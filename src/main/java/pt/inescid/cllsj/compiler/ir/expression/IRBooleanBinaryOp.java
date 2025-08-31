package pt.inescid.cllsj.compiler.ir.expression;

import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRBooleanBinaryOp extends IRBinaryOp {
  public IRBooleanBinaryOp(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public IRSlot getSlot() {
    return new IRBoolS();
  }
}
