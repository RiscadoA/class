package pt.inescid.cllsj.compiler.ir.type.slot;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRSlotT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRStringT extends IRSlotT {
  public IRStringT(IRType cont) {
    super(cont);
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "string; " + getCont();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRStringT(getCont().withContinuation(cont));
  }
}
