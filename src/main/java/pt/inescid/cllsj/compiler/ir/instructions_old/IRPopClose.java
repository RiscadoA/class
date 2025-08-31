package pt.inescid.cllsj.compiler.ir.instructions_old;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPopClose extends IRPop {
  public IRPopClose(int record, IRType recordType) {
    super(record, recordType);
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRPopClose(getRecord(), getRecordType());
  }

  @Override
  public String toString() {
    return toString("popClose");
  }
}
