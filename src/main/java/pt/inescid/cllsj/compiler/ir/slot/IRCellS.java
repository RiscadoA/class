package pt.inescid.cllsj.compiler.ir.slot;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRCellS extends IRSlot {
  private IRSlotTree slots;
  private IRValueRequisites isValue;

  public IRCellS(IRSlotTree slots, IRValueRequisites isValue) {
    this.slots = slots;
    this.isValue = isValue;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  public IRValueRequisites getIsValue() {
    return isValue;
  }

  @Override
  public IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> typeReplacer,
      Function<IRTypeId, IRValueRequisites> reqsReplacer) {
    return IRSlotTree.of(
        new IRCellS(
            slots.replaceTypes(typeReplacer, reqsReplacer), isValue.expandTypes(reqsReplacer)));
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "cell(" + slots + " (value=" + isValue + "))";
  }
}
