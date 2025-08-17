package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

// Instruction used by the flip forward optimization which merges a flip and a forward instruction
// into a single instruction.
public class IRFlipForward extends IRInstruction {
  private int negRecord; // Index of the record whose session is of a reading type.
  private int posRecord; // Index of the record whose session is of a writing type.

  public IRFlipForward(int negRecord, int posRecord) {
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
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "flipForward(-" + negRecord + ", +" + posRecord + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return this.negRecord == record || this.posRecord == record;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    negRecord = renamer.apply(negRecord);
    posRecord = renamer.apply(posRecord);
  }
}
