package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushUnfold extends IRPush {
  public IRPushUnfold(int record, IRType recordType) {
    super(record, recordType);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushUnfold");
  }

  @Override
  public IRInstruction clone() {
    return new IRPushUnfold(getRecord(), getRecordType());
  }
}
