package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;

public class IRWriteExpression extends IRWrite {
  private IRExpression expression;
  private boolean isExponential;

  public IRWriteExpression(int record, int slot, IRExpression expression, boolean isExponential) {
    super(record, slot);
    this.expression = expression;
    this.isExponential = isExponential;
  }

  public IRExpression getExpression() {
    return expression;
  }

  public boolean isExponential() {
    return isExponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "writeExpression("
        + getRecord() + ":" + getSlot()
        + ", "
        + expression
        + ", "
        + (isExponential ? "exponential" : "linear")
        + ")";
  }
}
