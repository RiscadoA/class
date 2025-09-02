package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public abstract class IRPop extends IRInstruction {
  private int record;
  private IRType recordType;

  public IRPop(int record, IRType recordType) {
    this.record = record;
    this.recordType = recordType;
  }

  public int getRecord() {
    return record;
  }

  public void setRecord(int record) {
    this.record = record;
  }

  public IRType getRecordType() {
    return recordType;
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

  public String toString(String popType) {
    return popType + "(" + record + "[" + recordType + "])";
  }

  public String toString(String popType, String arg) {
    return popType + "(" + record + "[" + recordType + "], " + arg + ")";
  }
}
