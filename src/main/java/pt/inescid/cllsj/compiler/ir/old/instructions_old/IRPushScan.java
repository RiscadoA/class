package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import pt.inescid.cllsj.compiler.ir.old.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;

public class IRPushScan extends IRPush {
  public IRPushScan(int record, IRType recordType) {
    super(record, recordType);
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushScan");
  }

  @Override
  public IRInstruction clone() {
    return new IRPushScan(getRecord(), getRecordType());
  }
}
