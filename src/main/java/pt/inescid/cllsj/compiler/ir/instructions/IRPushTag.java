package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRPushTag extends IRInstruction {
  private int record;
  private int tag;

  public IRPushTag(int record, int tag) {
    this.record = record;
    this.tag = tag;
  }

  public int getRecord() {
    return record;
  }

  public int getTag() {
    return tag;
  }

  @Override
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushTag(" + record + ", " + tag + ")";
  }
}
