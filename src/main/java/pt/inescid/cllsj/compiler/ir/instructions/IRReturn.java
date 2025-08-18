package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRReturn extends IRInstruction {
  private int record;
  private boolean isEndPoint = true;

  public IRReturn(int record) {
    this.record = record;
  }

  public int getRecord() {
    return record;
  }

  public boolean isEndPoint() {
    return isEndPoint;
  }

  public void removeEndPoint() {
    isEndPoint = false;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("return(");
    if (isEndPoint()) {
      sb.append("end point, ");
    }
    sb.append(record);
    sb.append(")");
    return sb.toString();
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
