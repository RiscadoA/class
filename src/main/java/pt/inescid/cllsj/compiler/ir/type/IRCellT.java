package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRCellT extends IRType {
  private IRType inner;

  public IRCellT(IRType inner) {
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
    return "cell " + inner.toString();
  }

  @Override
  public IRType removeLastSlot() {
    throw new UnsupportedOperationException("Cells are never meant to be removed from a type");
  }
}
