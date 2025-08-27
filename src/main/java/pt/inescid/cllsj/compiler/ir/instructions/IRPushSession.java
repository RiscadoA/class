package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushSession extends IRPush {
  private int argRecord;
  private IRType argRecordType;

  public IRPushSession(int record, IRType recordType, int argRecord, IRType argRecordType) {
    super(record, recordType);
    this.argRecord = argRecord;
    this.argRecordType = argRecordType;
  }

  public int getArgRecord() {
    return argRecord;
  }

  public IRType getArgRecordType() {
    return argRecordType;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("pushSession", Integer.toString(argRecord) + "[" + argRecordType + "]");
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    argRecord = renamer.apply(argRecord);
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    super.substituteTypes(types);
    argRecordType = types.apply(argRecordType);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushSession(getRecord(), getRecordType(), argRecord, argRecordType);
  }
}
