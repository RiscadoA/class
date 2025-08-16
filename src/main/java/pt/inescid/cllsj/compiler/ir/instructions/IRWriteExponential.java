package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRWriteExponential extends IRWrite {
  private int exponential;

  public IRWriteExponential(int record, int slot, int exponential) {
    super(record, slot);
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
    return "writeExponential(" + getRecord() + ":" + getSlot() + ", " + exponential + ")";
  }
}
