package pt.inescid.cllsj.compiler.ir.old.expressions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRExpressionVisitor;

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

  @Override
  public IRExpression clone() {
    return new IRInt(value);
  }

  public void renameRecords(Function<Integer, Integer> renamer) {}

  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
