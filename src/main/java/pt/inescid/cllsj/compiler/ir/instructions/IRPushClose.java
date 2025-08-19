package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushClose extends IRPush {
  public IRPushClose(int record) {
    super(record);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushClose(" + getRecord() + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRPushClose(getRecord());
  }
}
