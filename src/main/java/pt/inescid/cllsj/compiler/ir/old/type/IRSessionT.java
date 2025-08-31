package pt.inescid.cllsj.compiler.ir.old.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.old.IRTypeVisitor;

public class IRSessionT extends IRType {
  private IRType arg;
  private IRType cont;

  public IRSessionT(IRType arg, IRType cont) {
    this.arg = arg;
    this.cont = cont;
  }

  public IRType getArg() {
    return arg;
  }

  public IRType getCont() {
    return cont;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("session(");
    sb.append(arg.toString()).append("); ").append(cont.toString());
    return sb.toString();
  }

  @Override
  public ValueRequisites valueRequisites() {
    return cont.valueRequisites();
  }

  @Override
  public IRType leftmostTail() {
    return arg.leftmostTail();
  }

  @Override
  public IRType rightmostTail() {
    if (cont instanceof IRCloseT) {
      return this;
    } else {
      return cont.rightmostTail();
    }
  }

  @Override
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRSessionT(
        arg.substituteVar(index, offset, types), cont.substituteVar(index, offset, types));
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRSessionT
        && ((IRSessionT) other).getArg().equals(getArg())
        && ((IRSessionT) other).getCont().equals(getCont());
  }
}
