package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRIncRefExponential extends IRInstruction {
  private int exponential;

  public IRIncRefExponential(int exponential) {
    this.exponential = exponential;
  }

  public int getExponential() {
    return exponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "incRefExponential(" + exponential + ")";
  }
}
