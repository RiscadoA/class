package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRNewSession extends IRInstruction {
  private int record; // Index of the record to be initialized.
  private int size; // Size of the session's buffer in bytes.
  private String label; // Label for the initial continuation.

  public IRNewSession(int record, int size, String label) {
    this.record = record;
    this.size = size;
    this.label = label;
  }

  public int getRecord() {
    return record;
  }

  public int getSize() {
    return size;
  }

  public String getLabel() {
    return label;
  }

  @Override
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newSession(" + record + ", " + size + ", " + label + ")";
  }
}
