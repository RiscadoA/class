package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushExponential extends IRPush {
  private int exponential;

  public IRPushExponential(int record, int exponential) {
    super(record);
    this.exponential = exponential;
  }

  public int getExponential() {
    return exponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushExponential(" + getRecord() + ", " + exponential + ")";
  }
}
