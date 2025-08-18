package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRAnd extends IRExpression {
  private final IRExpression lhs;
  private final IRExpression rhs;

  public IRAnd(IRExpression lhs, IRExpression rhs) {
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
  public IRType getType() {
    return new IRBoolT();
  }

  @Override
  public String toString() {
    return "(" + lhs + " and " + rhs + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return lhs.usesRecord(record) || rhs.usesRecord(record);
  }

  public void renameRecords(Function<Integer, Integer> renamer) {
    lhs.renameRecords(renamer);
    rhs.renameRecords(renamer);
  }

  public void renameExponentials(Function<Integer, Integer> renamer) {
    lhs.renameExponentials(renamer);
    rhs.renameExponentials(renamer);
  }
}
