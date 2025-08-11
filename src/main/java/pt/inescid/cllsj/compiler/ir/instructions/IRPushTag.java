package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushTag extends IRPush {
  private int tag;

  public IRPushTag(int record, int tag) {
    super(record);
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
    return "pushTag(" + getRecord() + ", " + tag + ")";
  }
}
