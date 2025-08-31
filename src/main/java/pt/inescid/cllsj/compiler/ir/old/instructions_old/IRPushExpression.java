package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRExpression;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;

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
  public void accept(IRInstructionVisitorOld visitor) {
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
