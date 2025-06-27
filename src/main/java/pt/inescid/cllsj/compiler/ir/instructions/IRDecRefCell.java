package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRDecRefCell extends IRInstruction {
  private int record;

  public IRDecRefCell(int record) {
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
    return "decRefCell(" + record + ")";
  }
}
