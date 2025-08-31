package pt.inescid.cllsj.compiler.ir.expression.arithmetic;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRArithmeticBinaryOp;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;

public class IRMultiply extends IRArithmeticBinaryOp {
  public IRMultiply(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IRMultiply(lhs.clone(), rhs.clone());
  }

  @Override
  public String toString() {
    return "(" + lhs + " * " + rhs + ")";    
  }
}
