package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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
  public IRType getType() {
    return new IRIntT();
  }

  @Override
  public String toString() {
    return Integer.toString(value);
  }

  public void renameRecords(Function<Integer, Integer> renamer) {}

  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
