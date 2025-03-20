package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

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
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "branchOnPolarity(-" + negLabel + ", +" + posLabel + ")";
  }
}
