package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRNewExponentialScan extends IRInstruction {
  private int exponential;
  private IRType type;

  public IRNewExponentialScan(int exponential, IRType type) {
    this.exponential = exponential;
    this.type = type;
  }

  public int getExponential() {
    return exponential;
  }

  public IRType getType() {
    return type;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newExponentialScan(" + getExponential() + ", " + type + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRNewExponentialScan(exponential, type);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }
}
