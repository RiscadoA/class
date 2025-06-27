package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.List;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPutCell extends IRInstruction {
  private int record;
  private int argRecord;

  public IRPutCell(int record, int argRecord) {
    this.record = record;
    this.argRecord = argRecord;
  }

  public int getRecord() {
    return record;
  }

  public int getArgRecord() {
    return argRecord;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "putCell(" + record + ", " + argRecord + ")";
  }
}
