package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

// Serves as a marker that there's a polarity flip in the buffer.
public class IRFlipT extends IRType {
  IRType cont;

  public IRFlipT(IRType cont) {
    this.cont = cont;
  }

  public IRType getCont() {
    return cont;
  }

  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "flip; " + cont.toString();
  }
}
