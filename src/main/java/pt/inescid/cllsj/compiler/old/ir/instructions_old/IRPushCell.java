package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

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
  public void accept(IRInstructionVisitorOld visitor) {
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
