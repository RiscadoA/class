package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRRecT extends IRType {
  private IRType inner;

  public IRRecT(IRType inner) {
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
    return "rec " + inner.toString();
  }

  @Override
  public IRType removeLastSlot() {
    throw new UnsupportedOperationException("Recursive types recurse, and thus, don't have a last slot");
  }
}
