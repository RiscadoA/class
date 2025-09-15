package pt.inescid.cllsj.compiler.anl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class AnlFlowExponential {
  private int heapLocation;
  private Optional<List<AnlFlowSlot>> value = Optional.empty();

  public AnlFlowExponential(int heapLocation, Optional<List<AnlFlowSlot>> value) {
    this.heapLocation = heapLocation;
    this.value = value;
  }

  public int getHeapLocation() {
    return heapLocation;
  }

  public boolean hasKnownValue() {
    return value.isPresent();
  }

  public List<AnlFlowSlot> getValue() {
    return value.get();
  }

  public AnlFlowExponential clone() {
    // Never mutated, so no need to clone the value
    return this;
  }

  public AnlFlowExponential merge(AnlFlowExponential other) {
    if (other.value.isEmpty()) {
      return new AnlFlowExponential(heapLocation, Optional.empty());
    }

    if (other.value.get().size() != this.value.get().size()) {
      return new AnlFlowExponential(heapLocation, Optional.empty());
    }

    List<AnlFlowSlot> slots = new ArrayList<>();
    for (int i = 0; i < this.value.get().size(); ++i) {
      slots.add(this.value.get().get(i).merge(other.value.get().get(i)));
    }
    return new AnlFlowExponential(heapLocation, Optional.of(slots));
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (value.isEmpty()) {
      sb.append("?");
    } else {
      sb.append("[");
      for (int i = 0; i < value.get().size(); i++) {
        if (i > 0) {
          sb.append(", ");
        }
        sb.append(value.get().get(i));
      }
      sb.append("]");
    }
    return sb.toString();
  }
}
