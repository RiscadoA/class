package pt.inescid.cllsj.compiler.ir.expression;

import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public class IROrdinal extends IRUnaryOp {
  public IROrdinal(IRExpression str) {
    super(str);
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IROrdinal(inner);
  }

  @Override
  public IRSlot getSlot() {
    return new IRIntS();
  }

  @Override
  public String toString() {
    return "ord(" + inner.toString() + ")";
  }
}
