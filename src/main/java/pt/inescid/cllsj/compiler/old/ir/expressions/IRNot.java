package pt.inescid.cllsj.compiler.ir.old.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRExpressionVisitor;

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
  public IRExpression clone() {
    return new IRNot(inner.clone());
  }

  public void renameRecords(Function<Integer, Integer> renamer) {
    inner.renameRecords(renamer);
  }

  public void renameExponentials(Function<Integer, Integer> renamer) {
    inner.renameExponentials(renamer);
  }
}
