package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

// Instruction used by the flip forward optimization which merges a flip and a forward instruction
// into a single instruction.
public class IRFlipForward extends IRInstruction {
  private int negRecord; // Index of the record whose session is of a reading type.
  private int posRecord; // Index of the record whose session is of a writing type.
  private IRType type; // Type of the data being forwarded
  private boolean isEndPoint = true;

  public IRFlipForward(int negRecord, int posRecord, IRType type) {
    this.negRecord = negRecord;
    this.posRecord = posRecord;
    this.type = type;
  }

  public int getNegRecord() {
    return negRecord;
  }

  public int getPosRecord() {
    return posRecord;
  }

  public IRType getType() {
    return type;
  }

  public boolean isEndPoint() {
    return isEndPoint;
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
    StringBuilder sb = new StringBuilder("flipForward(");
    if (isEndPoint()) {
      sb.append("end point, ");
    }
    sb.append("-");
    sb.append(negRecord);
    sb.append(", +");
    sb.append(posRecord);
    sb.append(", ");
    sb.append(type);
    sb.append(")");
    return sb.toString();
  }

  @Override
  public IRInstruction clone() {
    IRFlipForward clone = new IRFlipForward(negRecord, posRecord, type);
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

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    type = types.apply(type);
  }
}
