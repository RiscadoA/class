package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitorOld;
import pt.inescid.cllsj.compiler.ir.type.IRExponentialT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushExponential extends IRPush {
  private int argExponential;

  public IRPushExponential(int record, IRType recordType, int argExponential) {
    super(record, recordType);
    this.argExponential = argExponential;
  }

  public int getArgExponential() {
    return argExponential;
  }

  public IRType getArgExponentialType() {
    if (!(getRecordType().leftmostTail() instanceof IRExponentialT)) {
      throw new IllegalStateException("Record type is not exponential");
    }

    return ((IRExponentialT) getRecordType().leftmostTail()).getInner();
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushExponential", Integer.toString(argExponential));
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    argExponential = renamer.apply(argExponential);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushExponential(getRecord(), getRecordType(), argExponential);
  }
}
