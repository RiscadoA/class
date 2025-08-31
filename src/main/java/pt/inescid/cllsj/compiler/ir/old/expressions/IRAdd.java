package pt.inescid.cllsj.compiler.ir.old.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.old.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.old.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;

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
  public IRType getType() {
    if (lhs.getType() instanceof IRIntT && rhs.getType() instanceof IRIntT) {
      return new IRIntT();
    } else if (lhs.getType() instanceof IRStringT || rhs.getType() instanceof IRStringT) {
      return new IRStringT();
    } else {
      throw new RuntimeException(
          "Unsupported type for add: " + lhs.getType() + " and " + rhs.getType());
    }
  }

  @Override
  public String toString() {
    return "(" + lhs + " + " + rhs + ")";
  }

  @Override
  public IRExpression clone() {
    return new IRAdd(lhs.clone(), rhs.clone());
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
