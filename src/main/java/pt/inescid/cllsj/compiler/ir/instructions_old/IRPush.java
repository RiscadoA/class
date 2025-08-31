package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public abstract class IRPush extends IRInstruction {
  private int record;
  private IRType recordType;

  public IRPush(int record, IRType recordType) {
    this.record = record;
    this.recordType = recordType;
  }

  public int getRecord() {
    return record;
  }

  public IRType getRecordType() {
    return recordType;
  }

  public void setRecord(int record) {
    this.record = record;
  }

  public void setRecordType(IRType recordType) {
    this.recordType = recordType;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
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

  public String toString(String pushType) {
    return pushType + "(" + record + "[" + recordType + "])";
  }

  public String toString(String pushType, String arg) {
    return pushType + "(" + record + "[" + recordType + "], " + arg + ")";
  }
}
