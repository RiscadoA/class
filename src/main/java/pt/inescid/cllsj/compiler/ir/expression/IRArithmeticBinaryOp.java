package pt.inescid.cllsj.compiler.ir.expression;

import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;

public abstract class IRArithmeticBinaryOp extends IRBinaryOp {
  public IRArithmeticBinaryOp(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public IRSlot getSlot() {
    if (lhs.getSlot() instanceof IRStringS || rhs.getSlot() instanceof IRStringS) {
      return new IRStringS();
    } else {
      return lhs.getSlot();
    }
  }
}
