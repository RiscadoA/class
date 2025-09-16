package pt.inescid.cllsj.compiler.anl;

import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Stream;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class AnlLocalDataState {
  // Known slots by their offset
  private Map<Integer, AnlSlot> slots = new TreeMap<>();

  public Stream<AnlSlot> read(IRSlotTree offset, Optional<IRSlotTree> size, boolean move) {
    int start = AnlSlot.count(offset);
    int end = start + size.map(s -> AnlSlot.count(s)).orElse(1);
    return Stream.iterate(start, i -> i + 1)
        .limit(end - start)
        .map(
            i -> {
              if (move) {
                return Optional.ofNullable(slots.remove(i)).orElse(AnlSlot.UNKNOWN);
              } else {
                return slots.computeIfAbsent(i, k -> AnlSlot.UNKNOWN);
              }
            });
  }

  public void write(IRSlotTree offset, Stream<AnlSlot> slots) {
    int i = AnlSlot.count(offset);
    for (AnlSlot slot : slots.toList()) {
      this.slots.put(i++, slot);
    }
  }

  public void markAsUnknown(Analyzer analyzer, AnlFlowState state) {
    for (AnlSlot slot : slots.values()) {
      slot.markAsUnknown(analyzer, state);
    }
    slots.clear();
  }

  public AnlLocalDataState clone() {
    AnlLocalDataState cloned = new AnlLocalDataState();
    for (Map.Entry<Integer, AnlSlot> entry : slots.entrySet()) {
      cloned.slots.put(entry.getKey(), entry.getValue().clone());
    }
    return cloned;
  }

  public AnlLocalDataState merge(AnlLocalDataState other) {
    AnlLocalDataState merged = this.clone();
    for (Map.Entry<Integer, AnlSlot> entry : other.slots.entrySet()) {
      if (!merged.slots.containsKey(entry.getKey())) {
        merged.slots.put(entry.getKey(), entry.getValue().clone());
      } else {
        merged.slots.put(entry.getKey(), merged.slots.get(entry.getKey()).merge(entry.getValue()));
      }
    }
    return merged;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (Map.Entry<Integer, AnlSlot> entry : slots.entrySet()) {
      sb.append(entry.getKey()).append("=").append(entry.getValue().toString());
    }
    if (sb.length() == 0) {
      sb.append("empty");
    }
    return sb.toString();
  }
}
