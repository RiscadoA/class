package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopType extends IRInstruction {
  private int record;
  private String positiveLabel;
  private String negativeLabel;

  public IRPopType(int record, String positiveLabel, String negativeLabel) {
    this.record = record;
    this.positiveLabel = positiveLabel;
    this.negativeLabel = negativeLabel;
  }

  public int getRecord() {
    return record;
  }

  public String getPositiveLabel() {
    return positiveLabel;
  }

  public String getNegativeLabel() {
    return negativeLabel;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "popType(" + record + ", +" + positiveLabel + ", -" + negativeLabel + ")";
  }
}
