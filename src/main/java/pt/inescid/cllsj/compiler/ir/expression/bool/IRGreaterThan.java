package pt.inescid.cllsj.compiler.ir.expression.bool;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRBooleanBinaryOp;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;

public class IRGreaterThan extends IRBooleanBinaryOp {
  public IRGreaterThan(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IRGreaterThan(lhs.clone(), rhs.clone());
  }

  @Override
  public String toString() {
    return "(" + lhs + " > " + rhs + ")";    
  }
}
