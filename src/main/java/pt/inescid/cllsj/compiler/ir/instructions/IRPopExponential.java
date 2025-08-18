package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopExponential extends IRPop {
  private int argExponential;

  public IRPopExponential(int record, int argExponential) {
    super(record);
    this.argExponential = argExponential;
  }

  public int getArgExponential() {
    return argExponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "popExponential(" + getRecord() + ", " + argExponential + ")";
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    argExponential = renamer.apply(argExponential);
  }
}
