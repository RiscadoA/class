package pt.inescid.cllsj.compiler.ir.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public abstract class IRType {
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  // Substitutes the variable with the given index with the given type.
  public abstract IRType substituteVar(
      int index, int offset, BiFunction<Integer, IRVarT, IRType> types);

  public abstract IRType substituteReqs(
      int offset, BiFunction<Integer, IRValueRequisites, IRValueRequisites> reqs);
}
