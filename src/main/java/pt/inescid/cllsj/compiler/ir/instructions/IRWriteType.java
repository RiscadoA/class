package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRWriteType extends IRWrite {
  private IRType type;
  private boolean isPositive;
  private IRValueRequisites valueRequisites;

  public IRWriteType(
      int record, int slot, IRType type, boolean isPositive, IRValueRequisites valueRequisites) {
    super(record, slot);
    this.type = type;
    this.isPositive = isPositive;
    this.valueRequisites = valueRequisites;
  }

  public IRType getType() {
    return type;
  }

  public boolean isPositive() {
    return isPositive;
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
    return "writeType("
        + getRecord() + ":" + getSlot()
        + ", "
        + type
        + ", "
        + (isPositive ? "positive" : "negative")
        + ", "
        + valueRequisites
        + ")";
  }
}
