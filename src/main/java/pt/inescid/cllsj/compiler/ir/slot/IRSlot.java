package pt.inescid.cllsj.compiler.ir.slot;

import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public abstract class IRSlot {
  public abstract void accept(IRSlotVisitor visitor);

  public IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> typeReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqsReplacer) {
    return IRSlotTree.of(this);
  }
}
