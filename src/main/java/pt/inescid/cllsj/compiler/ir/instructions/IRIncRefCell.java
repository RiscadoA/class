package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRIncRefCell extends IRInstruction {
  private int record;
  private int slot;

  public IRIncRefCell(int record, int slot) {
    this.record = record;
    this.slot = slot;
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
    return "incRefCell(" + record + ":" + slot + ")";
  }
}
