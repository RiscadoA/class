package pt.inescid.cllsj.compiler.ir.expression.arithmetic;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRArithmeticBinaryOp;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;

public class IRDivide extends IRArithmeticBinaryOp {
  public IRDivide(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IRDivide(lhs.clone(), rhs.clone());
  }

  @Override
  public String toString() {
    return "(" + lhs + " / " + rhs + ")";    
  }
}
