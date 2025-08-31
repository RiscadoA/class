package pt.inescid.cllsj.compiler.ir.expression;

import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRBinaryOp extends IRExpression {
  protected IRExpression lhs;
  protected IRExpression rhs;

  public IRBinaryOp(IRExpression lhs, IRExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public IRSlot getSlot() {
    return lhs.getSlot();
  }

  public IRExpression getLhs() {
    return lhs;
  }

  public IRExpression getRhs() {
    return rhs;
  }
}
