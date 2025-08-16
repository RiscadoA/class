package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.slot.IRBoolT;

public class IRBool extends IRExpression {
  private final boolean value;

  public IRBool(boolean value) {
    this.value = value;
  }

  public boolean getValue() {
    return value;
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
    return Boolean.toString(value);
  }
}
