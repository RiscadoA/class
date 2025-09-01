package pt.inescid.cllsj.compiler.ir.expression.literal;

import pt.inescid.cllsj.compiler.ir.expression.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRLiteral;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public class IRIntLiteral extends IRLiteral {
  private final int value;

  public IRIntLiteral(int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  @Override
  public IRSlot getSlot() {
    return new IRIntS();
  }

  @Override
  public String toString() {
    return Integer.toString(value);
  }

  @Override
  public IRIntLiteral clone() {
    return new IRIntLiteral(value);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }
}
