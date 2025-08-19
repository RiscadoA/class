package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRSleep extends IRInstruction {
  private int msecs;

  public IRSleep(int msecs) {
    this.msecs = msecs;
  }

  public int getMsecs() {
    return msecs;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "sleep(" + msecs + ")";
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}

  @Override
  public IRInstruction clone() {
    return new IRSleep(msecs);
  }
}
