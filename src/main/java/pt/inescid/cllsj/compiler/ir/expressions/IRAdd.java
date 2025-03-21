package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRAdd extends IRExpression {
  private final IRExpression lhs;
  private final IRExpression rhs;

  public IRAdd(IRExpression lhs, IRExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public IRExpression getLhs() {
    return lhs;
  }

  public IRExpression getRhs() {
    return rhs;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType() {
    return lhs.getType();
  }

  @Override
  public String toString() {
    return "(" + lhs + " + " + rhs + ")";
  }
}
