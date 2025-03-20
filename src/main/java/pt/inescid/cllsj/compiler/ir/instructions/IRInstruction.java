package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRInstruction {
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }
}
