package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRForward extends IRInstruction {
  private int negRecord; // Index of the record whose session is of a reading type.
  private int posRecord; // Index of the record whose session is of a writing type.
  
  // Used by the known jump optimization to avoid jumping after a forward
  // Happens when the continuation is known at compile time
  // In that case we just modify the posRecord accordingly and delete the negRecord
  private boolean shouldReturn = true;

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

  public boolean shouldReturn() {
    return shouldReturn;
  }

  public void removeReturn() {
    this.shouldReturn = false;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("forward(");
    sb.append("-").append(negRecord);
    sb.append(", +").append(posRecord);
    if (!shouldReturn) {
      sb.append(", no return");
    }
    sb.append(")");
    return sb.toString();
  }
}
