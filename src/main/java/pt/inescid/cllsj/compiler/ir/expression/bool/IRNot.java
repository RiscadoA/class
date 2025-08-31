package pt.inescid.cllsj.compiler.ir.expression.bool;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.expression.IRUnaryOp;

public class IRNot extends IRUnaryOp {
  public IRNot(IRExpression inner) {
    super(inner);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IRNot(inner.clone());
  }

  @Override
  public String toString() {
    return "not(" + inner + ")";    
  }
}
