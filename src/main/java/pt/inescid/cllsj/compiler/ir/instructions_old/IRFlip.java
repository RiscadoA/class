package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRFlip extends IRInstruction {
  private int record;
  private IRType recordType;
  private String contLabel;

  public IRFlip(int record, IRType recordType, String contLabel) {
    this.record = record;
    this.recordType = recordType;
    this.contLabel = contLabel;
  }

  public int getRecord() {
    return record;
  }

  public IRType getRecordType() {
    return recordType;
  }

  public String getContLabel() {
    return contLabel;
  }

  public void setContLabel(String contLabel) {
    this.contLabel = contLabel;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "flip(" + record + "[" + recordType + "], " + contLabel + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRFlip(record, recordType, contLabel);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}

  @Override
  public void renameLabels(Function<String, String> renamer) {
    contLabel = renamer.apply(contLabel);
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    recordType = types.apply(recordType);
  }
}
