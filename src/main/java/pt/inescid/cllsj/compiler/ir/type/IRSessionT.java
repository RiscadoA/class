package pt.inescid.cllsj.compiler.ir.type;

import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public class IRSessionT extends IRType {
  private IRType arg;
  private IRType cont;
  private IRValueRequisites valueRequisites;

  public IRSessionT(IRType arg, IRType cont, IRValueRequisites valueRequisites) {
    this.arg = arg;
    this.cont = cont;
    this.valueRequisites = valueRequisites;
  }

  public IRType getArg() {
    return arg;
  }

  public IRType getCont() {
    return cont;
  }

  public IRValueRequisites getValueRequisites() {
    return valueRequisites;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("session(");
    if (valueRequisites.canBeValue()) {
      sb.append(valueRequisites.toString()).append(": ");
    }
    sb.append(arg.toString()).append("); ").append(cont.toString());
    return sb.toString();
  }

  @Override
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    return new IRSessionT(
        arg.substituteVar(index, offset, types),
        cont.substituteVar(index, offset, types),
        valueRequisites);
  }

  @Override
  public IRType substituteReqs(
      int offset, BiFunction<Integer, IRValueRequisites, IRValueRequisites> reqs) {
    return new IRSessionT(
        arg.substituteReqs(offset, reqs),
        cont.substituteReqs(offset, reqs),
        reqs.apply(offset, valueRequisites));
  }
}
