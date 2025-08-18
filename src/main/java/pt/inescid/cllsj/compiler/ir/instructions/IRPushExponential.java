package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushExponential extends IRPush {
  private int exponential;

  public IRPushExponential(int record, int exponential) {
    super(record);
    this.exponential = exponential;
  }

  public int getExponential() {
    return exponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushExponential(" + getRecord() + ", " + exponential + ")";
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }
}
