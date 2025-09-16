package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRWriteCell extends IRWrite {
  private IRSlotTree slots;

  public IRWriteCell(IRDataLocation location, IRSlotTree slots) {
    super(location);
    this.slots = slots;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteCell(location, slots);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("writeCell(");
    sb.append(location).append(", ");
    sb.append(slots).append(")");
    return sb.toString();
  }

  @Override
  public void replaceSlots(Function<IRSlotTree, IRSlotTree> replacer) {
    super.replaceSlots(replacer);
    slots = replacer.apply(slots);
  }
}
