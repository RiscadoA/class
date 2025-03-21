package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRExponentialT extends IRType {
  private IRType cont;

  public IRExponentialT(IRType cont) {
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
    return "exponential; " + cont;
  }
}
