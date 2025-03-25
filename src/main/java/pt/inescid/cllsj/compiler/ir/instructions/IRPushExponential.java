package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushExponential extends IRInstruction {
  private int record;
  private String processName;

  public IRPushExponential(int record, String processName) {
    this.record = record;
    this.processName = processName;
  }

  public int getRecord() {
    return record;
  }

  public String getProcessName() {
    return processName;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushExponential(" + record + ", " + processName + ")";
  }
}
