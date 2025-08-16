package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRReadExponential extends IRRead {
  private int argExponential;

  public IRReadExponential(int record, int slot, int argExponential) {
    super(record, slot);
    this.argExponential = argExponential;
  }

  public int getArgExponential() {
    return argExponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "readExponential(" + getRecord() + ":" + getSlot() + ", " + argExponential + ")";
  }
}
