package pt.inescid.cllsj.compiler.ir.slot;

import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRVarS extends IRSlot {
  private IRTypeId typeId;

  public IRVarS(IRTypeId typeId) {
    this.typeId = typeId;
  }

  public IRTypeId getTypeId() {
    return typeId;
  }

  @Override
  public void accept(IRSlotVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return typeId.toString();
  }

  @Override
  public IRSlotTree replaceTypes(
      Function<IRTypeId, IRSlotTree> typeReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqsReplacer) {
    return typeReplacer.apply(typeId);
  }
}
