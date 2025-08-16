package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPush extends IRInstruction {
  private int record;

  public IRPush(int record) {
    this.record = record;
  }

  public int getRecord() {
    return record;
  }

  public void setRecord(int record) {
    this.record = record;
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
