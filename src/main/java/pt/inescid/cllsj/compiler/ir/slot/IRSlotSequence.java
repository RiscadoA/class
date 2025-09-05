package pt.inescid.cllsj.compiler.ir.slot;

import java.util.ArrayList;
import java.util.List;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;

public class IRSlotSequence {
  private List<IRSlot> slots;

  public static final IRSlotSequence EMPTY = new IRSlotSequence(List.of());

  public IRSlotSequence(List<IRSlot> slots) {
    this.slots = slots;
  }

  public static IRSlotSequence of(IRSlot... slots) {
    return new IRSlotSequence(List.of(slots));
  }

  public List<IRSlot> list() {
    return slots;
  }

  public IRSlot get(int index) {
    return slots.get(index);
  }

  public int size() {
    return slots.size();
  }

  public IRSlotSequence prefix(IRSlot slot) {
    List<IRSlot> newSlots = new ArrayList<>();
    newSlots.add(slot);
    newSlots.addAll(slots);
    return new IRSlotSequence(newSlots);
  }

  public IRSlotSequence prefix(IRSlotSequence other) {
    List<IRSlot> newSlots = new ArrayList<>(other.slots);
    newSlots.addAll(slots);
    return new IRSlotSequence(newSlots);
  }

  public IRSlotSequence suffix(IRSlot slot) {
    return suffix(new IRSlotSequence(List.of(slot)));
  }

  public IRSlotSequence suffix(IRSlotSequence other) {
    List<IRSlot> newSlots = new ArrayList<>(slots);
    newSlots.addAll(other.slots);
    return new IRSlotSequence(newSlots);
  }

  public void instantiateTypeVariable(IRTypeId typeId, IRSlotSequence sequence) {
    for (int i = 0; i < slots.size(); ++i) {
      if (!(slots.get(i) instanceof IRVarS)) {
        continue;
      }

      IRVarS slot = (IRVarS) slots.get(i);
      if (!slot.getTypeId().equals(typeId)) {
        continue;
      }

      slots.remove(i);
      slots.addAll(i, sequence.list());
    }
  }

  public IRSlotSequence clone() {
    return new IRSlotSequence(new ArrayList<>(slots));
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[");
    if (!slots.isEmpty()) {
      sb.append(slots.getFirst().toString());
    }
    for (int i = 1; i < slots.size(); i++) {
      sb.append("; ").append(slots.get(i).toString());
    }
    sb.append("]");
    return sb.toString();
  }
}
