package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRNewSession extends IRInstruction {
  private int record; // Index of the record to be initialized.
  private String label; // Label for the initial continuation.

  public IRNewSession(int record, String label) {
    this.record = record;
    this.label = label;
  }

  public int getRecord() {
    return record;
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
    return "newSession(" + record + ", " + label + ")";
  }
}
