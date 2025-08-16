package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRWriteTag extends IRWrite {
  private int tag;

  public IRWriteTag(int record, int slot, int tag) {
    super(record, slot);
    this.tag = tag;
  }

  public int getTag() {
    return tag;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "writeTag(" + getRecord() + ":" + getSlot() + ", " + tag + ")";
  }
}
