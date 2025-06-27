package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushCell extends IRInstruction {
  private int record;
  private int argRecord;

  public IRPushCell(int record, int argRecord) {
    this.record = record;
    this.argRecord = argRecord;
  }

  public int getRecord() {
    return record;
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
    return "pushCell(" + record + ", " + argRecord + ")";
  }
}
