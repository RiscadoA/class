package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRPopSession extends IRInstruction {
  private int record;
  private int argRecord; // Index where the new record will be stored.

  public IRPopSession(int record, int argRecord) {
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
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "popSession(" + record + ", " + argRecord + ")";
  }
}
