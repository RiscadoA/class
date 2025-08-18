package pt.inescid.cllsj.compiler.ir.expressions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRVar extends IRExpression {
  private int record;
  private final IRType type;

  public IRVar(int record, IRType type) {
    this.record = record;
    this.type = type;
  }

  public int getRecord() {
    return record;
  }

  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRType getType() {
    return type;
  }

  @Override
  public String toString() {
    return "record " + record;
  }

  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
