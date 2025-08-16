package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRNewThread extends IRInstruction {
  private String label;

  public IRNewThread(String label) {
    this.label = label;
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
    return "newThread(" + label + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return false;
  }
}
