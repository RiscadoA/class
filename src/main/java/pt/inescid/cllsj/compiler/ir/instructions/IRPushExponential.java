package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushExponential extends IRInstruction {
  private int record;
  private int parentCount;
  private int recordCount;
  private String label;

  public IRPushExponential(int record, int parentCount, int recordCount, String label) {
    this.record = record;
    this.parentCount = parentCount;
    this.recordCount = recordCount;
    this.label = label;
  }

  public int getRecord() {
    return record;
  }

  public int getParentCount() {
    return parentCount;
  }

  public int getRecordCount() {
    return recordCount;
  }

  public String getLabel() {
    return label;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushExponential("
        + record
        + ", "
        + parentCount
        + ", "
        + recordCount
        + ", "
        + label
        + ")";
  }
}
