package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newExponentialScan(" + getExponential() + ", " + type + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return false;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }
}
