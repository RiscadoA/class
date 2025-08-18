package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRNot extends IRExpression {
  private final IRExpression inner;

  public IRNot(IRExpression inner) {
    this.inner = inner;
  }

  public IRExpression getInner() {
    return inner;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType() {
    return new IRBoolT();
  }

  @Override
  public String toString() {
    return "not(" + inner + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return inner.usesRecord(record);
  }

  public void renameRecords(Function<Integer, Integer> renamer) {
    inner.renameRecords(renamer);
  }

  public void renameExponentials(Function<Integer, Integer> renamer) {
    inner.renameExponentials(renamer);
  }
}
