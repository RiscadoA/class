package pt.inescid.cllsj.compiler.ir.expression.arithmetic;

import pt.inescid.cllsj.compiler.ir.expression.IRArithmeticBinaryOp;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.expression.IRExpressionVisitor;

public class IRModulus extends IRArithmeticBinaryOp {
  public IRModulus(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IRModulus(lhs.clone(), rhs.clone());
  }

  @Override
  public String toString() {
    return "(" + lhs + " % " + rhs + ")";
  }
}
