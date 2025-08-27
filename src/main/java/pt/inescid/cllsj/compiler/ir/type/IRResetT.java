package pt.inescid.cllsj.compiler.ir.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

// Serves as a marker that there's a polarity flip in the buffer.
public class IRResetT extends IRType {
  private IRType cont;

  public IRResetT(IRType cont) {
    this.cont = cont;
  }

  public IRType getCont() {
    return cont;
  }

  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "reset; " + cont.toString();
  }

  @Override
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRResetT(cont.substituteVar(index, offset, types));
  }

  @Override
  public ValueRequisites valueRequisites() {
    return ValueRequisites.notValue();
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRResetT && ((IRResetT) other).getCont().equals(getCont());
  }
}
