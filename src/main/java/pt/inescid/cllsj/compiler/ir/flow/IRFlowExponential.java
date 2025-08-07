package pt.inescid.cllsj.compiler.ir.flow;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class IRFlowExponential {
  private Optional<List<IRFlowSlot>> value = Optional.empty();

  public IRFlowExponential() {}

  public IRFlowExponential(List<IRFlowSlot> value) {
    this.value = Optional.of(value);
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

  public IRFlowExponential merge(IRFlowState.Cloner cloner, IRFlowExponential other) {
    if (other.value.isEmpty()) {
      this.value = Optional.empty();
      return new IRFlowExponential();
    }

    if (other.value.get().size() != this.value.get().size()) {
      value = Optional.empty();
      return new IRFlowExponential();
    }

    List<IRFlowSlot> slots = new ArrayList<>();
    for (int i = 0; i < this.value.get().size(); ++i) {
      slots.add(this.value.get().get(i).merge(cloner, other.value.get().get(i)));
    }
    return new IRFlowExponential(slots);
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
