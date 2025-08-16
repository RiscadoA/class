package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPop extends IRInstruction {
  private int record;

  public IRPop(int record) {
    this.record = record;
  }

  public int getRecord() {
    return record;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public boolean usesRecord(int record) {
    return this.record == record;
  }
}
