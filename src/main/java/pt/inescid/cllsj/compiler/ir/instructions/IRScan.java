package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRScan extends IRPush {
  private IRType type;

  public IRScan(int record, IRType type) {
    super(record);
    this.type = type;
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
    return "scan(" + getRecord() + ", " + type + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRScan(getRecord(), type);
  }
}
