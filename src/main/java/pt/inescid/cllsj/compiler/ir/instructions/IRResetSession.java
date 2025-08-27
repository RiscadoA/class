package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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
  public void accept(IRInstructionVisitor visitor) {
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
