package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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

  public void renameRecords(Function<Integer, Integer> renamer) {}

  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
