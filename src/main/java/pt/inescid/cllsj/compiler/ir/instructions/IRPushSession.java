package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushSession extends IRPush {
  private int argRecord;
  private IRValueRequisites valueRequisites;

  public IRPushSession(int record, int argRecord, IRValueRequisites valueRequisites) {
    super(record);
    this.argRecord = argRecord;
    this.valueRequisites = valueRequisites;
  }

  public int getArgRecord() {
    return argRecord;
  }

  public IRValueRequisites getValueRequisites() {
    return valueRequisites;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushSession(" + getRecord() + ", " + argRecord + ", " + valueRequisites + ")";
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    argRecord = renamer.apply(argRecord);
  }

  @Override
  public void substituteTypes(
      Function<IRType, IRType> types, Function<IRValueRequisites, IRValueRequisites> requisites) {
    this.valueRequisites = requisites.apply(this.valueRequisites);
  }

  @Override
  public IRInstruction clone() {
    return new IRPushSession(getRecord(), argRecord, valueRequisites.clone());
  }
}
