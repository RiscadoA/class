package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopExponential extends IRInstruction {
  private int record;
  private int argExponential;

  public IRPopExponential(int record, int argExponential) {
    this.record = record;
  }

  public int getRecord() {
    return record;
  }

  public int getArgExponential() {
    return argExponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "popExponential(" + record + ", " + argExponential + ")";
  }
}
