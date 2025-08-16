package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public abstract class IRType {
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  // Returns a new type in which the last slot in the type's 'buffer' has been removed.
  public abstract IRType removeLastSlot();
}
