package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRWriteCell extends IRWrite {
  private int argRecord;

  public IRWriteCell(int record, int slot, int argRecord) {
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
    return "writeCell(" + getRecord() + ":" + getSlot() + ", " + argRecord + ")";
  }
}
