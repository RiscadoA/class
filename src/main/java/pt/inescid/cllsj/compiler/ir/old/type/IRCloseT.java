package pt.inescid.cllsj.compiler.ir.old.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.old.IRTypeVisitor;

public class IRCloseT extends IRType {
  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "close";
  }

  @Override
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return this;
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRCloseT;
  }
}
