package pt.inescid.cllsj.compiler.ir.slot;

import java.util.ArrayList;
import java.util.List;

public class IRSlotSequence {
  private List<IRSlot> slots;

  public static final IRSlotSequence EMPTY = new IRSlotSequence(List.of());

  public IRSlotSequence(List<IRSlot> slots) {
    this.slots = slots;
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

  public boolean isPolymorphic() {
    return slots.stream().anyMatch(s -> s instanceof IRVarS);
  }

  public void instantiateTypeVariable(int typeId, IRSlotSequence sequence) {
    for (int i = 0; i < slots.size(); ++i) {
      if (!(slots.get(i) instanceof IRVarS)) {
        continue;
      }

      IRVarS slot = (IRVarS) slots.get(i);
      if (slot.getTypeId() != typeId) {
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
    if (!slots.isEmpty()) {
      sb.append(slots.getFirst().toString());
    }
    for (int i = 1; i < slots.size(); i++) {
      sb.append("; ").append(slots.get(i).toString());
    }
    return sb.toString();
  }
}
