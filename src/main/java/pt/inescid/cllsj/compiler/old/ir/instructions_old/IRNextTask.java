package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRNextTask extends IRInstruction {
  private boolean isEndPoint = true;

  public boolean isEndPoint() {
    return isEndPoint;
  }

  public void removeEndPoint() {
    isEndPoint = false;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    IRNextTask clone = new IRNextTask();
    clone.isEndPoint = this.isEndPoint;
    return clone;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("nextTask(");
    if (isEndPoint()) {
      sb.append("end point");
    }
    sb.append(")");
    return sb.toString();
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
