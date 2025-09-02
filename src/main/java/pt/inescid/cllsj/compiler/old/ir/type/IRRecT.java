package pt.inescid.cllsj.compiler.ir.old.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.old.ir.IRTypeVisitor;

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
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRRecT(inner.substituteVar(index + 1, offset + 1, types));
  }

  @Override
  public ValueRequisites valueRequisites() {
    return ValueRequisites.notValue();
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRRecT && ((IRRecT) other).getInner().equals(getInner());
  }
}
