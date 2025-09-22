package pt.inescid.cllsj.compiler.ir.slot;

import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRCellS extends IRSlot {
  private IRSlotTree slots;

  public IRCellS(IRSlotTree slots) {
    this.slots = slots;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  @Override
  public IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> typeReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqsReplacer) {
    return IRSlotTree.of(new IRCellS(slots.replaceTypes(typeReplacer, reqsReplacer)));
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("cell[");
    slots.toStringHelper(sb);
    sb.append("]");
    return sb.toString();
  }
}
