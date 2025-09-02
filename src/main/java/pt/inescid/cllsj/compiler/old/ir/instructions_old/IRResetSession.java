package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRResetSession extends IRInstruction {
  private int record;
  private IRType recordType;

  public IRResetSession(int record, IRType recordType) {
    this.record = record;
    this.recordType = recordType;
  }

  public int getRecord() {
    return record;
  }

  public IRType getRecordType() {
    return recordType;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "resetSession(" + record + "[" + recordType + "])";
  }

  @Override
  public IRInstruction clone() {
    return new IRResetSession(record, recordType);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    recordType = types.apply(recordType);
  }
}
