package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRFreeSession extends IRInstruction {
  private int record;

  public IRFreeSession(int record) {
    this.record = record;
  }

  public int getRecord() {
    return record;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "freeSession(" + record + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRFreeSession(record);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
