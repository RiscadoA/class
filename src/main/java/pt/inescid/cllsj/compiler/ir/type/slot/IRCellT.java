package pt.inescid.cllsj.compiler.ir.type.slot;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRSlotT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRCellT extends IRSlotT {
  private IRType inner;

  public IRCellT(IRType inner, IRType cont) {
    super(cont);
    this.inner = inner;
  }

  public IRType getInner() {
    return inner;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "cell " + inner.toString() + "; " + getCont();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRCellT(inner.withContinuation(cont), getCont().withContinuation(cont));
  }
}
