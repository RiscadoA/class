package pt.inescid.cllsj.compiler.ir.slot;

import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRCellS extends IRSlot {
  private IRSlotTree slots;
  private IRTypeFlagRequisites isValue;

  public IRCellS(IRSlotTree slots, IRTypeFlagRequisites isValue) {
    this.slots = slots;
    this.isValue = isValue;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  public IRTypeFlagRequisites getIsValue() {
    return isValue;
  }

  @Override
  public IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> typeReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqsReplacer) {
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
    if (isValue.isPossible()) {
      return "cell[" + slots + " (value=" + isValue + ")]";
    } else {
      return "cell[affine]";
    }
  }
}
