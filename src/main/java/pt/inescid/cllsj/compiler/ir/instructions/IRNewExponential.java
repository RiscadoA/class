package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRNewExponential extends IRInstruction {
  private int exponential;
  private int record;

  public IRNewExponential(int exponential, int record) {
    this.exponential = exponential;
    this.record = record;
  }

  public int getExponential() {
    return exponential;
  }

  public int getRecord() {
    return record;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newExponential(" + exponential + ", " + record + ")";
  }
}
