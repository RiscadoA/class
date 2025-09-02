package pt.inescid.cllsj.compiler.ir.old.expressions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRExpressionVisitor;

public class IRString extends IRExpression {
  private final String value;

  public IRString(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  public String getEscapedValue() {
    String value = this.value;
    value = value.replace("\"", "\\\"");
    value = value.replace("\n", "\\n");
    value = value.replace("\t", "\\t");
    return value;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType() {
    return new IRStringT();
  }

  @Override
  public String toString() {
    return "\"" + getEscapedValue() + "\"";
  }

  @Override
  public IRExpression clone() {
    return new IRString(value);
  }

  public void renameRecords(Function<Integer, Integer> renamer) {}

  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
