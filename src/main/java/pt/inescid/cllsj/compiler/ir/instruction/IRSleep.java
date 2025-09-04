package pt.inescid.cllsj.compiler.ir.instruction;

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
  public IRInstruction clone() {
    return new IRSleep(msecs);
  }

  @Override
  public String toString() {
    return "sleep(" + msecs + ")";
  }
}
