package pt.inescid.cllsj.compiler.ir.expression.bool;

import pt.inescid.cllsj.compiler.ir.expression.IRBooleanBinaryOp;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.expression.IRExpressionVisitor;

public class IREqual extends IRBooleanBinaryOp {
  public IREqual(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IREqual(lhs.clone(), rhs.clone());
  }

  @Override
  public String toString() {
    return "(" + lhs + " = " + rhs + ")";
  }
}
