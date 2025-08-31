package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushExpression extends IRPush {
  private IRExpression expression;
  private boolean isExponential;

  public IRPushExpression(
      int record, IRType recordType, IRExpression expression, boolean isExponential) {
    super(record, recordType);
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
    return toString(
        "pushExpression", expression + ", " + (isExponential ? "exponential" : "linear"));
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    expression.renameRecords(renamer);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    super.renameExponentials(renamer);
    expression.renameExponentials(renamer);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushExpression(getRecord(), getRecordType(), expression.clone(), isExponential);
  }
}
