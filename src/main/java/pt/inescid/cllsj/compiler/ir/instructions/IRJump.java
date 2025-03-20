package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRJump extends IRInstruction {
  private String label;

  public IRJump(String label) {
    this.label = label;
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
    return "jump(" + label + ")";
  }
}
