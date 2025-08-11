package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushUnfold extends IRPush {
  public IRPushUnfold(int record) {
    super(record);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushUnfold(" + getRecord() + ")";
  }
}
