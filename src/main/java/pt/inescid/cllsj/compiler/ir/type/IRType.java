package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public abstract class IRType {
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }
}
