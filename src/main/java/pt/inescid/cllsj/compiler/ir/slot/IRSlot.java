package pt.inescid.cllsj.compiler.ir.slot;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public abstract class IRSlot {
  public abstract void accept(IRSlotVisitor visitor);

  public IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> typeReplacer,
      Function<IRTypeId, IRValueRequisites> reqsReplacer) {
    return IRSlotTree.of(this);
  }
}
