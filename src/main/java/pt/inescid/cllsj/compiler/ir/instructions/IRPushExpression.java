package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;

public class IRPushExpression extends IRPush {
  private IRExpression expression;
  private boolean isExponential;

  public IRPushExpression(int record, IRExpression expression, boolean isExponential) {
    super(record);
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
    return "pushExpression("
        + getRecord()
        + ", "
        + expression
        + ", "
        + (isExponential ? "exponential" : "linear")
        + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return super.usesRecord(record) || expression.usesRecord(record);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    expression.renameRecords(renamer);
  }
}
