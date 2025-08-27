package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushType extends IRPush {
  private int argRecord;
  private IRType argType;
  private boolean argIsPositive;

  public IRPushType(
      int record, IRType recordType, int argRecord, IRType argType, boolean argIsPositive) {
    super(record, recordType);
    this.argRecord = argRecord;
    this.argType = argType;
    this.argIsPositive = argIsPositive;
  }

  public int getArgRecord() {
    return argRecord;
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
        "pushType", argRecord + ", " + argType + ", " + (argIsPositive ? "positive" : "negative"));
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    argRecord = renamer.apply(argRecord);
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    super.substituteTypes(types);
    argType = types.apply(argType);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushType(getRecord(), getRecordType(), argRecord, argType, argIsPositive);
  }
}
