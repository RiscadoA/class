package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public class IRPopSession extends IRPop {
  private int argRecord; // Index where the new record will be stored.
  private IRValueRequisites valueRequisites;

  public IRPopSession(int record, int argRecord, IRValueRequisites valueRequisites) {
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
    return "popSession(" + getRecord() + ", " + argRecord + ", " + valueRequisites + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return super.usesRecord(record) || this.getArgRecord() == record;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    argRecord = renamer.apply(argRecord);
  }
}
