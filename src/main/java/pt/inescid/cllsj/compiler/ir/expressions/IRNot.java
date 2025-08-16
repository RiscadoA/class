package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.slot.IRBoolT;

public class IRNot extends IRExpression {
  private final IRExpression inner;

  public IRNot(IRExpression inner) {
    this.inner = inner;
  }

  public IRExpression getInner() {
    return inner;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType(IRType cont) {
    return new IRBoolT(cont);
  }

  @Override
  public String toString() {
    return "not(" + inner + ")";
  }
}
