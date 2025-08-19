package pt.inescid.cllsj.compiler.ir.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

// Serves as a marker that there's a polarity flip in the buffer.
public class IRFlipT extends IRType {
  private IRType cont;

  public IRFlipT(IRType cont) {
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
    return "flip; " + cont.toString();
  }

  @Override
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRFlipT(cont.substituteVar(index, offset, types));
  }

  @Override
  public IRType substituteReqs(
      int offset, BiFunction<Integer, IRValueRequisites, IRValueRequisites> reqs) {
    return new IRFlipT(cont.substituteReqs(offset, reqs));
  }
}
