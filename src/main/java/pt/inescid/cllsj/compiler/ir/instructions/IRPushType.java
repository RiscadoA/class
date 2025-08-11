package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPushType extends IRPush {
  private IRType type;
  private boolean isPositive;
  private IRValueRequisites valueRequisites;

  public IRPushType(
      int record, IRType type, boolean isPositive, IRValueRequisites valueRequisites) {
    super(record);
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
    return "pushType("
        + getRecord()
        + ", "
        + type
        + ", "
        + (isPositive ? "positive" : "negative")
        + ", "
        + valueRequisites
        + ")";
  }
}
