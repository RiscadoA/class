package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRSleep extends IRInstruction {
  private int msecs;

  public IRSleep(int msecs) {
    this.msecs = msecs;
  }

  public int getMsecs() {
    return msecs;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "sleep(" + msecs + ")";
  }
}
