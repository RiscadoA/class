package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRPushTag extends IRPush {
  private int tag;

  public IRPushTag(int record, IRType recordType, int tag) {
    super(record, recordType);
    this.tag = tag;
  }

  public int getTag() {
    return tag;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushTag", Integer.toString(tag));
  }

  @Override
  public IRInstruction clone() {
    return new IRPushTag(getRecord(), getRecordType(), tag);
  }
}
