package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRFlip extends IRInstruction {
  private int record;
  private String contLabel;

  public IRFlip(int record, String contLabel) {
    this.record = record;
    this.contLabel = contLabel;
  }

  public int getRecord() {
    return record;
  }

  public String getContLabel() {
    return contLabel;
  }

  public void setContLabel(String contLabel) {
    this.contLabel = contLabel;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "flip(" + record + ", " + contLabel + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRFlip(record, contLabel);
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
}
