package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRBoolT extends IRType {
  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "bool";
  }

  @Override
  public IRType removeLastSlot() {
    return new IRCloseT();
  }
}
