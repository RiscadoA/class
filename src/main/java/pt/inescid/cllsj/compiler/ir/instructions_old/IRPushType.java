package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRTypeT;

public class IRPushType extends IRPush {
  private int contRecord;
  private IRType argType;
  private boolean argIsPositive;

  public IRPushType(
      int record, IRType recordType, int contRecord, IRType argType, boolean argIsPositive) {
    super(record, recordType);
    this.contRecord = contRecord;
    this.argType = argType;
    this.argIsPositive = argIsPositive;
  }

  public int getContRecord() {
    return contRecord;
  }

  public IRType getContRecordType() {
    if (!(getRecordType() instanceof IRTypeT)) {
      throw new UnsupportedOperationException("Record must be of type IRTypeT");
    }
    IRTypeT type = (IRTypeT) getRecordType();
    return type.getCont();
  }

  public IRType getArgType() {
    return argType;
  }

  public boolean isArgPositive() {
    return argIsPositive;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString(
        "pushType", contRecord + ", " + argType + ", " + (argIsPositive ? "positive" : "negative"));
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    contRecord = renamer.apply(contRecord);
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    super.substituteTypes(types);
    argType = types.apply(argType);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushType(getRecord(), getRecordType(), contRecord, argType, argIsPositive);
  }
}
