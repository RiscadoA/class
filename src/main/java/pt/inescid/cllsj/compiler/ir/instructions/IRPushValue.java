package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

// Consumes a session record and pushes its data into an existing session record.
public class IRPushValue extends IRInstruction {
  private int record;
  private int argRecord; // Index of the consumed recod.

  public IRPushValue(int record, int argRecord) {
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
    return "pushValue(" + record + ", " + argRecord + ")";
  }
}
