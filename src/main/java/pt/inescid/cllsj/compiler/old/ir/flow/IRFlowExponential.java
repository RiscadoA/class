package pt.inescid.cllsj.compiler.ir.old.flow;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class IRFlowExponential {
  private int heapLocation;
  private Optional<List<IRFlowSlot>> value = Optional.empty();

  public IRFlowExponential(int heapLocation, Optional<List<IRFlowSlot>> value) {
    this.heapLocation = heapLocation;
    this.value = value;
  }

  public int getHeapLocation() {
    return heapLocation;
  }

  public boolean hasKnownValue() {
    return value.isPresent();
  }

  public List<IRFlowSlot> getValue() {
    return value.get();
  }

  public IRFlowExponential clone() {
    // Never mutated, so no need to clone the value
    return this;
  }

  public IRFlowExponential merge(IRFlowExponential other) {
    if (other.value.isEmpty()) {
      return new IRFlowExponential(heapLocation, Optional.empty());
    }

    if (other.value.get().size() != this.value.get().size()) {
      return new IRFlowExponential(heapLocation, Optional.empty());
    }

    List<IRFlowSlot> slots = new ArrayList<>();
    for (int i = 0; i < this.value.get().size(); ++i) {
      slots.add(this.value.get().get(i).merge(other.value.get().get(i)));
    }
    return new IRFlowExponential(heapLocation, Optional.of(slots));
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
