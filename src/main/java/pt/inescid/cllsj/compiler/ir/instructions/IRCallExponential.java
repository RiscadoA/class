package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRCallExponential extends IRInstruction {
  private int exponential;
  private int argRecord;

  public IRCallExponential(int exponential, int argRecord) {
    this.exponential = exponential;
    this.argRecord = argRecord;
  }

  public int getExponential() {
    return exponential;
  }

  public int getArgRecord() {
    return argRecord;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "callExponential(" + exponential + ", " + argRecord + ")";
  }
}
