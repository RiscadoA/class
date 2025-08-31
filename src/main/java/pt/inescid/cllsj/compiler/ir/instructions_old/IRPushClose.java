package pt.inescid.cllsj.compiler.ir.instructions_old;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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
