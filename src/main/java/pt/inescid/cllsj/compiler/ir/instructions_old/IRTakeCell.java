package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRTakeCell extends IRInstruction {
  private int record;
  private int argRecord; // Index where the new record will be stored.

  public IRTakeCell(int record, int argRecord) {
    this.record = record;
    this.argRecord = argRecord;
  }

  public int getRecord() {
    return record;
  }

  public int getArgRecord() {
    return argRecord;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "takeCell(" + record + ", " + argRecord + ")";
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
    argRecord = renamer.apply(argRecord);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}

  @Override
  public IRInstruction clone() {
    return new IRTakeCell(record, argRecord);
  }
}
