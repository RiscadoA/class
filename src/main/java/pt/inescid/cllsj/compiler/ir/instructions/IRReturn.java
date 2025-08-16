package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRReturn extends IRInstruction {
  private int record;

  public IRReturn(int record) {
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
  public String toString() {
    return "return(" + record + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return this.record == record;
  }
}
