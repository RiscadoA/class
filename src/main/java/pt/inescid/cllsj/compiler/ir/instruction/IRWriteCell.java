package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRWriteCell extends IRWrite {
  private IRSlotTree slots;
  private IRProcessId managerId;
  private List<IRWriteExponential.TypeArgument> managerTypeArguments;

  public IRWriteCell(
      IRDataLocation location,
      IRSlotTree slots,
      IRProcessId managerId,
      List<IRWriteExponential.TypeArgument> managerTypeArguments) {
    super(location);
    this.slots = slots;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  public IRProcessId getManagerId() {
    return managerId;
  }

  public List<IRWriteExponential.TypeArgument> getManagerTypeArguments() {
    return managerTypeArguments;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteCell(location, slots, managerId, managerTypeArguments);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("writeCell(").append(location).append(", ");
    sb.append(slots).append(", ").append(managerId);
    for (IRWriteExponential.TypeArgument typeArg : managerTypeArguments) {
      sb.append(", ").append(typeArg.toString());
    }
    sb.append(")");
    return sb.toString();
  }
}
