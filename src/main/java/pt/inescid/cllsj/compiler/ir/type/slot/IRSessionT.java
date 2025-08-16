package pt.inescid.cllsj.compiler.ir.type.slot;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRSlotT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRSessionT extends IRSlotT {
  private IRType arg;

  public IRSessionT(IRType arg, IRType cont) {
    super(cont);
    this.arg = arg;
  }

  public IRType getArg() {
    return arg;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("session(");
    sb.append(arg.toString()).append("); ").append(getCont());
    return sb.toString();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRSessionT(arg.withContinuation(cont), getCont().withContinuation(cont));
  }
}
