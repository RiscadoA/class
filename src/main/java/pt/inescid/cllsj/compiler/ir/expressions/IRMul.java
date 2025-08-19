package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRMul extends IRExpression {
  private final IRExpression lhs;
  private final IRExpression rhs;

  public IRMul(IRExpression lhs, IRExpression rhs) {
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
    return new IRIntT();
  }

  @Override
  public String toString() {
    return "(" + lhs + " * " + rhs + ")";
  }

  @Override
  public IRExpression clone() {
    return new IRMul(lhs.clone(), rhs.clone());
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
