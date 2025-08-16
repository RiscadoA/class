package pt.inescid.cllsj.compiler.ir.type.slot;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRSlotT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRBoolT extends IRSlotT {
  public IRBoolT(IRType cont) {
    super(cont);
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "bool; " + getCont();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRBoolT(getCont().withContinuation(cont));
  }
}
