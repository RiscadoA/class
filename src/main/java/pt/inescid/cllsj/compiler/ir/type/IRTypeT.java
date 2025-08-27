package pt.inescid.cllsj.compiler.ir.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRTypeT extends IRType {
  private IRType cont;

  public IRTypeT(IRType cont) {
    this.cont = cont;
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
    return "type(" + cont + ")";
  }

  @Override
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRTypeT(cont.substituteVar(index + 1, offset + 1, types));
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRTypeT && ((IRTypeT) other).getCont().equals(getCont());
  }
}
