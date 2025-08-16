package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.slot.IRStringT;
import pt.inescid.cllsj.compiler.ir.type.IRCloseT;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.slot.IRIntT;

public class IRAdd extends IRExpression {
  private final IRExpression lhs;
  private final IRExpression rhs;

  public IRAdd(IRExpression lhs, IRExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  public IRExpression getLhs() {
    return lhs;
  }

  public IRExpression getRhs() {
    return rhs;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType(IRType cont) {
    if (lhs.getType(cont) instanceof IRIntT && rhs.getType(cont) instanceof IRIntT) {
      return new IRIntT(new IRCloseT());
    } else if (lhs.getType(cont) instanceof IRStringT || rhs.getType(cont) instanceof IRStringT) {
      return new IRStringT(new IRCloseT());
    } else {
      throw new RuntimeException(
          "Unsupported type for add: " + lhs.getType(cont) + " and " + rhs.getType(cont));
    }
  }

  @Override
  public String toString() {
    return "(" + lhs + " + " + rhs + ")";
  }
}
