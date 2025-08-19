package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRNewExponential extends IRInstruction {
  private int exponential;
  private int record;

  public IRNewExponential(int exponential, int record) {
    this.exponential = exponential;
    this.record = record;
  }

  public int getExponential() {
    return exponential;
  }

  public int getRecord() {
    return record;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newExponential(" + exponential + ", " + record + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRNewExponential(exponential, record);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }
}
