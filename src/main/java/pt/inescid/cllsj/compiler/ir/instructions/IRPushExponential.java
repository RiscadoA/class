package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushExponential extends IRPush {
  private int exponential;

  public IRPushExponential(int record, IRType recordType, int exponential) {
    super(record, recordType);
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
    return toString("pushExponential", Integer.toString(exponential));
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushExponential(getRecord(), getRecordType(), exponential);
  }
}
