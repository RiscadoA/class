package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRInstruction {
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }
}
