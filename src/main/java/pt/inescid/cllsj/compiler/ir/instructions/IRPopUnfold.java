package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPopUnfold extends IRPop {
  public IRPopUnfold(int record, IRType recordType) {
    super(record, recordType);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("popUnfold");
  }

  @Override
  public IRInstruction clone() {
    return new IRPopUnfold(getRecord(), getRecordType());
  }
}
