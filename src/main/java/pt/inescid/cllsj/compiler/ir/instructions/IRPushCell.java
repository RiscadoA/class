package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushCell extends IRPush {
  private int argRecord;

  public IRPushCell(int record, int argRecord) {
    super(record);
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
    return "pushCell(" + getRecord() + ", " + argRecord + ")";
  }
}
