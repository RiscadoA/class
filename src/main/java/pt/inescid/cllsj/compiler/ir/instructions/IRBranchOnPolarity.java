package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRBranchOnPolarity extends IRInstruction {
  private int type;
  private String negLabel;
  private String posLabel;

  public IRBranchOnPolarity(int type, String negLabel, String posLabel) {
    this.type = type;
    this.negLabel = negLabel;
    this.posLabel = posLabel;
  }

  public int getType() {
    return type;
  }

  public String getNegLabel() {
    return negLabel;
  }

  public String getPosLabel() {
      return posLabel;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "branchOnPolarity<var " + type + ">(-" + negLabel + ", +" + posLabel + ")";
  }
}
