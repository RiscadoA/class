package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRForward extends IRInstruction {
  private int negRecord; // Index of the record whose session is of a reading type.
  private int posRecord; // Index of the record whose session is of a writing type.

  public IRForward(int negRecord, int posRecord) {
    this.negRecord = negRecord;
    this.posRecord = posRecord;
  }

  public int getNegRecord() {
    return negRecord;
  }

  public int getPosRecord() {
    return posRecord;
  }

  @Override
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "forward(-" + negRecord + ", +" + posRecord + ")";
  }
}
