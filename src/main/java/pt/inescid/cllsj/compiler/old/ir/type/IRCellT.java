package pt.inescid.cllsj.compiler.ir.old.type;

import java.util.function.BiFunction;

import pt.inescid.cllsj.compiler.old.ir.IRTypeVisitor;

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
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRCellT(inner.substituteVar(index, offset, types));
  }

  @Override
  public ValueRequisites valueRequisites() {
    return ValueRequisites.notValue();
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRCellT && ((IRCellT) other).getInner().equals(getInner());
  }
}
