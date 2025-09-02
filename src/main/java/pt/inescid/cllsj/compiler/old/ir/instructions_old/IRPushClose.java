package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRPushClose extends IRPush {
  public IRPushClose(int record, IRType recordType) {
    super(record, recordType);
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushClose");
  }

  @Override
  public IRInstruction clone() {
    return new IRPushClose(getRecord(), getRecordType());
  }
}
