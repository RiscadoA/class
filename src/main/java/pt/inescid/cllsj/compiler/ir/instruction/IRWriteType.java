package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRTypeFlag;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRWriteType extends IRWrite {
  private IRSlotTree slots;
  private IRTypeFlagRequisites valueRequisites;
  private IRTypeFlagRequisites cloneableRequisites;
  private IRTypeFlagRequisites droppableRequisites;

  public IRWriteType(
      IRDataLocation location,
      IRSlotTree typeSlots,
      IRTypeFlagRequisites valueRequisites,
      IRTypeFlagRequisites cloneableRequisites,
      IRTypeFlagRequisites droppableRequisites) {
    super(location);
    this.slots = typeSlots;
    this.valueRequisites = valueRequisites;
    this.cloneableRequisites = cloneableRequisites;
    this.droppableRequisites = droppableRequisites;
  }

  public IRSlotTree getSlots() {
    return slots;
  }

  public Map<IRTypeFlag, IRTypeFlagRequisites> getFlags() {
    Map<IRTypeFlag, IRTypeFlagRequisites> flags = new HashMap<>();
    flags.put(IRTypeFlag.IS_VALUE, valueRequisites);
    flags.put(IRTypeFlag.IS_CLONEABLE, cloneableRequisites);
    flags.put(IRTypeFlag.IS_DROPPABLE, droppableRequisites);
    return flags;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteType(
        location, slots, valueRequisites, cloneableRequisites, droppableRequisites);
  }

  @Override
  public String toString() {
    return "writeType("
        + location.toString()
        + ", "
        + slots.toString()
        + ", value="
        + valueRequisites
        + ", cloneable="
        + cloneableRequisites
        + ", droppable="
        + droppableRequisites
        + ")";
  }

  @Override
  public void replaceTypes(
      Function<IRTypeId, IRSlotTree> slotReplacer,
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer) {
    slots = slots.replaceTypes(slotReplacer, reqReplacer);
    valueRequisites = valueRequisites.expandTypes(reqReplacer);
    cloneableRequisites = cloneableRequisites.expandTypes(reqReplacer);
    droppableRequisites = droppableRequisites.expandTypes(reqReplacer);
  }
}
