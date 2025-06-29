package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRScan extends IRInstruction {
  private int record;
  private IRType type;

  public IRScan(int record, IRType type) {
    this.record = record;
    this.type = type;
  }

  public int getRecord() {
    return record;
  }

  public IRType getType() {
    return type;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "scan(" + record + ", " + type + ")";
  }
}
