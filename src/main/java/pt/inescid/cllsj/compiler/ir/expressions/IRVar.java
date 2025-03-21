package pt.inescid.cllsj.compiler.ir.expressions;

import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRVar extends IRExpression {
  private final int record;
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
}
