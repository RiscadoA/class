package pt.inescid.cllsj.compiler.ir.type;

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
}
