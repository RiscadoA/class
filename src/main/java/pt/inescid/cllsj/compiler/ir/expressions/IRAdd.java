package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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

  public void renameRecords(Function<Integer, Integer> renamer) {
    lhs.renameRecords(renamer);
    rhs.renameRecords(renamer);
  }

  public void renameExponentials(Function<Integer, Integer> renamer) {
    lhs.renameExponentials(renamer);
    rhs.renameExponentials(renamer);
  }
}
