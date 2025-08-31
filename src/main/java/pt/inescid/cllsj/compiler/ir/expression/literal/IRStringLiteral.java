package pt.inescid.cllsj.compiler.ir.expression.literal;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRLiteral;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;

public class IRStringLiteral extends IRLiteral {
  private final String value;

  public IRStringLiteral(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

  @Override
  public IRSlot getSlot() {
    return new IRStringS();
  }

  @Override
  public String toString() {
    return "\"" + value + "\"";
  }

  @Override
  public IRStringLiteral clone() {
    return new IRStringLiteral(value);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }
}
