package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.slot.IRIntT;

public class IRInt extends IRExpression {
  private final int value;

  public IRInt(int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType(IRType cont) {
    return new IRIntT(cont);
  }

  @Override
  public String toString() {
    return Integer.toString(value);
  }
}
