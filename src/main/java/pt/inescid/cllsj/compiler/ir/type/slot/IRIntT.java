package pt.inescid.cllsj.compiler.ir.type.slot;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRSlotT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRIntT extends IRSlotT {
  public IRIntT(IRType cont) {
    super(cont);
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "int; " + getCont();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRIntT(getCont().withContinuation(cont));
  }
}
