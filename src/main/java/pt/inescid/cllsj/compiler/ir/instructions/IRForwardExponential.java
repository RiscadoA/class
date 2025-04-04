package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRForwardExponential extends IRInstruction {
  private int record;
  private int exponential;

  public IRForwardExponential(int record, int exponential) {
    this.record = record;
    this.exponential = exponential;
  }

  public int getRecord() {
    return record;
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
    return "forwardExponential(" + record + ", " + exponential + ")";
  }
}
