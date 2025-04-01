package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRNewSession extends IRInstruction {
  private int record; // Index of the record to be initialized.
  private IRType type; // Type of the session, used to determine buffer size.
  private String label; // Label for the initial continuation.

  public IRNewSession(int record, IRType type, String label) {
    this.record = record;
    this.type = type;
    this.label = label;
  }

  public int getRecord() {
    return record;
  }

  public IRType getType() {
    return type;
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
    return "newSession<" + type + ">(" + record + ", " + label + ")";
  }
}
