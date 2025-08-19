package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRForward extends IRInstruction {
  private int negRecord; // Index of the record whose session is of a reading type.
  private int posRecord; // Index of the record whose session is of a writing type.

  // Used by the known jump optimization to avoid jumping after a forward
  // Happens when the continuation is known at compile time
  // In that case we just modify the posRecord accordingly and delete the negRecord
  private boolean shouldReturn = true;

  private boolean isEndPoint = true;

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

  public boolean isEndPoint() {
    return shouldReturn && isEndPoint;
  }

  public void removeEndPoint() {
    isEndPoint = false;
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
    } else if (isEndPoint()) {
      sb.append(", end point");
    }
    sb.append(")");
    return sb.toString();
  }

  @Override
  public IRInstruction clone() {
    IRForward clone = new IRForward(negRecord, posRecord);
    clone.shouldReturn = shouldReturn;
    clone.isEndPoint = isEndPoint;
    return clone;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    negRecord = renamer.apply(negRecord);
    posRecord = renamer.apply(posRecord);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
