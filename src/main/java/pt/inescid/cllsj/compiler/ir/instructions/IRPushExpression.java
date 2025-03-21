package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;

public class IRPushExpression extends IRInstruction {
  private int record;
  private IRExpression expression;

  public IRPushExpression(int record, IRExpression expression) {
    this.record = record;
    this.expression = expression;
  }

  public int getRecord() {
    return record;
  }

  public IRExpression getExpression() {
    return expression;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushExpression(" + record + ", " + expression + ")";
  }
}
