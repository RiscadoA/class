package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushCell extends IRPush {
  private int argRecord;

  public IRPushCell(int record, IRType recordType, int argRecord) {
    super(record, recordType);
    this.argRecord = argRecord;
  }

  public int getArgRecord() {
    return argRecord;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushCell", Integer.toString(argRecord));
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    argRecord = renamer.apply(argRecord);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushCell(getRecord(), getRecordType(), argRecord);
  }
}
