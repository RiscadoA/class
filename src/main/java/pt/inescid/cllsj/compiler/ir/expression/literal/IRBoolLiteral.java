package pt.inescid.cllsj.compiler.ir.expression.literal;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRLiteral;
import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public class IRBoolLiteral extends IRLiteral {
  private final boolean value;

  public IRBoolLiteral(boolean value) {
    this.value = value;
  }

  public boolean getValue() {
    return value;
  }

  @Override
  public IRSlot getSlot() {
    return new IRBoolS();
  }

  @Override
  public String toString() {
    return Boolean.toString(value);
  }

  @Override
  public IRBoolLiteral clone() {
    return new IRBoolLiteral(value);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }
}
