package pt.inescid.cllsj.compiler.ir.type;

import java.util.Optional;
import java.util.function.Function;

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
  public IRType removeLastSlot() {
    if (cont instanceof IRCloseT) {
      return new IRCloseT();
    } else {
      return new IRSessionT(arg, cont.removeLastSlot(), valueRequisites);
    }
  }
}
