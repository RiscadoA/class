package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRDetachExponential extends IRInstruction {
  private int exponential;

  public IRDetachExponential(int exponential) {
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
    return "detachExponential(" + exponential + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return false;
  }
}
