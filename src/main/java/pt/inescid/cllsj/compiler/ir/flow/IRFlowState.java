package pt.inescid.cllsj.compiler.ir.flow;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;

import pt.inescid.cllsj.compiler.ir.type.IRFlipT;
import pt.inescid.cllsj.compiler.ir.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.type.IRTagT;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRVarT;

public class IRFlowState {
  private Map<Integer, IRFlowRecord> boundRecords = new HashMap<>();
  private Map<Integer, IRFlowExponential> boundExponentials = new HashMap<>();
  private Map<Integer, IRFlowType> boundTypes = new HashMap<>();
  private Optional<Stack<String>> pendingContinuations = Optional.of(new Stack<>());

  public static class Cloner {
    private Map<IRFlowRecord, IRFlowRecord> recordMap = new HashMap<>();

    public Optional<IRFlowRecord> getCloned(IRFlowRecord record) {
      return Optional.ofNullable(recordMap.get(record));
    }

    public void setCloned(IRFlowRecord original, IRFlowRecord clone) {
      recordMap.put(original, clone);
    }
  }

  public IRFlowRecord record(int id) {
    return boundRecords.computeIfAbsent(id, k -> new IRFlowRecord(id));
  }

  public IRFlowExponential exponential(int id) {
    return boundExponentials.computeIfAbsent(id, k -> new IRFlowExponential());
  }

  public IRFlowType type(int id) {
    return boundTypes.computeIfAbsent(id, k -> new IRFlowType());
  }

  public void bindType(int id, IRFlowType type) {
    boundTypes.put(id, type);
  }

  public void bindExponential(int id, IRFlowExponential exponential) {
    boundExponentials.put(id, exponential);
  }

  public void unbindExponential(int id) {
    boundExponentials.remove(id);
  }

  public void bindRecord(int id, IRFlowRecord record) {
    boundRecords.put(id, record);
  }

  public void unbindRecord(int id) {
    boundRecords.remove(id);
  }

  public void pushPendingContinuation(String continuation) {
    pendingContinuations.get().push(continuation);
  }

  public Optional<String> popPendingContinuation() {
    if (pendingContinuations.get().isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(pendingContinuations.get().pop());
  }

  public Optional<Stack<String>> getPendingContinuations() {
    return pendingContinuations;
  }

  public Optional<Integer> slotCount(IRType type) {
    if (type instanceof IRSessionT) {
      IRSessionT sessionType = (IRSessionT) type;
      Optional<Integer> contSlots = slotCount(sessionType.getCont());

      if (sessionType.getValueRequisites().mustBeValue()) {
        Optional<Integer> valueSlots = slotCount(sessionType.getArg());
        if (!contSlots.isPresent() || !valueSlots.isPresent()) {
          return Optional.empty();
        }
        return Optional.of(contSlots.get() + valueSlots.get());
      } else if (sessionType.getValueRequisites().canBeValue()) {
        return Optional.empty();
      } else {
        return contSlots;
      }
    } else if (type instanceof IRFlipT) {
      return slotCount(((IRFlipT) type).getCont());
    } else if (type instanceof IRVarT) {
      int id = ((IRVarT) type).getType();
      if (type(id).getType().isPresent()) {
        return slotCount(type(id).getType().get());
      } else {
        return Optional.empty();  
      }
    } else if (type instanceof IRTagT) {
      return Optional.empty();
    } else {
      return Optional.of(1);
    }
  }

  public IRFlowState merge(IRFlowState state) {
    Cloner cloner = new Cloner();
    IRFlowState merged = this.clone();
    for (int index : state.boundRecords.keySet()) {
      merged.record(index).merge(cloner, state.record(index));
    }
    for (int index : state.boundExponentials.keySet()) {
      merged.exponential(index).merge(cloner, state.exponential(index));
    }
    for (int index : state.boundTypes.keySet()) {
      if (merged.boundTypes.containsKey(index)) {
        merged.bindType(index, merged.boundTypes.get(index).merge(state.boundTypes.get(index)));
      } else {
        merged.bindType(index, state.boundTypes.get(index));
      }
    }
    if (!this.pendingContinuations.equals(state.pendingContinuations)) {
      merged.pendingContinuations = Optional.empty();
    }
    return merged;
  }

  public IRFlowState clone() {
    Cloner cloner = new Cloner();
    IRFlowState clone = new IRFlowState();
    for (Map.Entry<Integer, IRFlowRecord> entry : this.boundRecords.entrySet()) {
      clone.boundRecords.put(entry.getKey(), entry.getValue().clone(cloner));
    }
    for (Map.Entry<Integer, IRFlowExponential> entry : this.boundExponentials.entrySet()) {
      clone.boundExponentials.put(entry.getKey(), entry.getValue().clone());
    }
    for (Map.Entry<Integer, IRFlowType> entry : this.boundTypes.entrySet()) {
      clone.boundTypes.put(entry.getKey(), entry.getValue().clone());
    }
    if (this.pendingContinuations.isEmpty()) {
      clone.pendingContinuations = Optional.empty();
    } else {
      clone.pendingContinuations.get().addAll(this.pendingContinuations.get());      
    }
    return clone;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (Map.Entry<Integer, IRFlowRecord> entry : boundRecords.entrySet()) {
      sb.append("record ")
          .append(entry.getKey())
          .append(": ")
          .append(entry.getValue())
          .append("\n");
    }
    for (Map.Entry<Integer, IRFlowExponential> entry : boundExponentials.entrySet()) {
      sb.append("exponential ")
          .append(entry.getKey())
          .append(": ")
          .append(entry.getValue())
          .append("\n");
    }
    for (Map.Entry<Integer, IRFlowType> entry : boundTypes.entrySet()) {
      sb.append("type ")
          .append(entry.getKey())
          .append(": ")
          .append(entry.getValue())
          .append("\n");
    }
    if (pendingContinuations.isEmpty()) {
      sb.append("pending: ?\n");
    } else {
      for (String pending : pendingContinuations.get()) {
        sb.append("pending: ").append(pending).append("\n");
      }
    }
    return sb.toString();
  }
}
