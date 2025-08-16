package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRDecRefCell extends IRInstruction {
  private int record;
  private int slot;

  public IRDecRefCell(int record, int slot) {
    this.record = record;
    this.slot = slot;
  }

  public int getRecord() {
    return record;
  }

  public int getSlot() {
    return slot;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "decRefCell(" + record + ":" + slot + ")";
  }
}
