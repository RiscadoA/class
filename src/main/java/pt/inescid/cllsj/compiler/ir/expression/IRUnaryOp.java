package pt.inescid.cllsj.compiler.ir.expression;

import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRUnaryOp extends IRExpression {
  protected IRExpression inner;

  public IRUnaryOp(IRExpression inner) {
    this.inner = inner;
  }

  @Override
  public IRSlot getSlot() {
    return inner.getSlot();
  }

  public IRExpression getInner() {
    return inner;
  }
}
