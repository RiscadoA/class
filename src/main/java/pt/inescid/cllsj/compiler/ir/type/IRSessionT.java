package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

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
    return "session(" + arg + "); " + cont;
  }
}
