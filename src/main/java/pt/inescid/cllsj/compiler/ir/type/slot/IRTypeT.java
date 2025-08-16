package pt.inescid.cllsj.compiler.ir.type.slot;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRSlotT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRTypeT extends IRSlotT {
  private IRType inner;

  public IRTypeT(IRType inner, IRType cont) {
    super(cont);
    this.inner = inner;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "type(" + inner + "); " + getCont();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRTypeT(inner.withContinuation(cont), getCont().withContinuation(cont));
  }
}
