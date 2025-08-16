package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public abstract class IRExpression {
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  public abstract IRType getType();

  public abstract boolean usesRecord(int record);
}
