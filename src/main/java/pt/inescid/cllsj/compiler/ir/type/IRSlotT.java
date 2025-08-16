package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public abstract class IRSlotT extends IRType {
  private IRType cont;

  public IRSlotT(IRType cont) {
    this.cont = cont;
  }

  public IRType getCont() {
    return cont;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }
}
