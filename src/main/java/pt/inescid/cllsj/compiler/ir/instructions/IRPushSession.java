package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public class IRPushSession extends IRInstruction {
  private int record;
  private int argRecord;
  private IRValueRequisites valueRequisites;

  public IRPushSession(int record, int argRecord, IRValueRequisites valueRequisites) {
    this.record = record;
    this.argRecord = argRecord;
    this.valueRequisites = valueRequisites;
  }

  public int getRecord() {
    return record;
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
    return "pushSession(" + record + ", " + argRecord + ", " + valueRequisites + ")";
  }
}
