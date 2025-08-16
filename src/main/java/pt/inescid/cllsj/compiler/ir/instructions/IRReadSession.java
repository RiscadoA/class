package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRReadSession extends IRRead {
  private int argRecord; // Index where the new record will be stored.

  public IRReadSession(int record, int slot, int argRecord) {
    super(record, slot);
    this.argRecord = argRecord;
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
    return "readSession(" + getRecord() + ":" + getSlot() + ", " + argRecord + ")";
  }
}
