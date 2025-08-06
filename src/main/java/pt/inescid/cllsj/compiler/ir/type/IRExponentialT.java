package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRExponentialT extends IRType {
  private IRType inner;

  public IRExponentialT(IRType inner) {
    this.inner = inner;
  }

  public IRType getInner() {
    return inner;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "exponential(" + inner.toString() + ")";
  }
}
