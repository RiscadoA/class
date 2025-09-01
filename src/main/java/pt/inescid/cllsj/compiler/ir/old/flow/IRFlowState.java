package pt.inescid.cllsj.compiler.ir.old.flow;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;
import pt.inescid.cllsj.compiler.ir.old.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.old.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.old.type.IRCellT;
import pt.inescid.cllsj.compiler.ir.old.type.IRCloseT;
import pt.inescid.cllsj.compiler.ir.old.type.IRExponentialT;
import pt.inescid.cllsj.compiler.ir.old.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.old.type.IRRecT;
import pt.inescid.cllsj.compiler.ir.old.type.IRResetT;
import pt.inescid.cllsj.compiler.ir.old.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.old.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.old.type.IRTagT;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.ir.old.type.IRTypeT;
import pt.inescid.cllsj.compiler.ir.old.type.IRVarT;
import pt.inescid.cllsj.compiler.old.IRAnalyzer;

public class IRFlowState {
  private static int nextExponentialLocation = 0;

  private Map<IRFlowLocation, IRFlowRecord> recordHeap = new HashMap<>();
  private Map<Integer, IRFlowExponential> exponentialHeap = new HashMap<>();

  private Map<Integer, IRFlowLocation> boundRecords = new HashMap<>();
  private Map<Integer, Integer> boundExponentials = new HashMap<>();
  private Map<Integer, IRFlowType> boundTypes = new HashMap<>();
  private Optional<Stack<IRFlowContinuation>> pendingContinuations = Optional.of(new Stack<>());

  public IRFlowRecord allocateRecord(IRFlowLocation location) {
    if (recordHeap.containsKey(location)) {
      throw new UnsupportedOperationException("Record has been already allocated");
    }

    IRFlowRecord record = new IRFlowRecord(location);
    recordHeap.put(location, record);
    return record;
  }

  public void freeRecord(IRFlowRecord record) {
    recordHeap.remove(record.getIntroductionLocation());
  }

  public IRFlowExponential allocateExponential(Optional<List<IRFlowSlot>> value) {
    int location = nextExponentialLocation++;
    IRFlowExponential exponential = new IRFlowExponential(location, value);
    exponentialHeap.put(location, exponential);
    return exponential;
  }

  public void freeExponential(int location) {
    exponentialHeap.remove(location);
  }

  public IRFlowRecord getHeapRecord(IRFlowLocation location) {
    return recordHeap.get(location);
  }

  public IRFlowRecord getBoundRecord(int id) {
    return recordHeap.get(boundRecords.get(id));
  }

  public IRFlowExponential getBoundExponential(int id) {
    return exponentialHeap.get(boundExponentials.get(id));
  }

  public IRFlowType boundType(int id) {
    return boundTypes.computeIfAbsent(id, k -> new IRFlowType());
  }

  public void bindType(int id, IRFlowType type) {
    boundTypes.put(id, type);
  }

  public void bindExponential(int id, IRFlowExponential exponential) {
    bindExponential(id, exponential.getHeapLocation());
  }

  public void bindExponential(int id, int location) {
    boundExponentials.put(id, location);
  }

  public void unbindExponential(int id) {
    boundExponentials.remove(id);
  }

  public void bindRecord(int id, IRFlowRecord record) {
    bindRecord(id, record.getIntroductionLocation());
  }

  public void bindRecord(int id, IRFlowLocation location) {
    boundRecords.put(id, location);
  }

  public void unbindRecord(int id) {
    boundRecords.remove(id);
  }

  public void pushPendingContinuation(IRAnalyzer analyzer, IRFlowContinuation cont) {
    analyzer.getFlow().addDetached(analyzer.getFlow(cont.getLabel()));
    pendingContinuations.get().push(cont);
  }

  public void pushPendingContinuation(IRAnalyzer analyzer, String label, IRFlowLocation location) {
    pushPendingContinuation(analyzer, new IRFlowContinuation(label, location));
  }

  public Optional<IRFlowContinuation> popPendingContinuation() {
    if (pendingContinuations.get().isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(pendingContinuations.get().pop());
  }

  public Optional<Stack<IRFlowContinuation>> getPendingContinuations() {
    return pendingContinuations;
  }

  public IRFlowState merge(IRAnalyzer analyzer, IRFlowLocation location, IRFlowState other) {
    if (this == other) {
      return this;
    }

    IRFlowState merged = this.clone();
    for (Map.Entry<IRFlowLocation, IRFlowRecord> entry : other.recordHeap.entrySet()) {
      if (merged.recordHeap.containsKey(entry.getKey())) {
        IRFlowRecord result = merged.recordHeap.get(entry.getKey()).merge(entry.getValue());
        merged.recordHeap.put(entry.getKey(), result);
      } else {
        merged.recordHeap.put(entry.getKey(), entry.getValue().clone());
      }
    }
    for (Map.Entry<Integer, IRFlowLocation> entry : other.boundRecords.entrySet()) {
      if (merged.boundRecords.containsKey(entry.getKey())) {
        IRFlowLocation mergedLoc = merged.boundRecords.get(entry.getKey());
        IRFlowLocation otherLoc = entry.getValue();
        if (mergedLoc != otherLoc) {
          if (mergedLoc.isKnown() || otherLoc.isKnown()) {
            throw new IllegalArgumentException(
                "Cannot merge states which bind different records ("
                    + mergedLoc
                    + " != "
                    + otherLoc
                    + ") to the same binding ("
                    + entry.getKey()
                    + ")");
          }

          // If two different records are bound to the same binding,
          // we get rid of both of them and create a new record
          IRFlowRecord thisRecord = merged.recordHeap.get(mergedLoc);
          IRFlowRecord otherRecord = merged.recordHeap.get(otherLoc);
          IRFlowRecord newRecord = thisRecord.merge(otherRecord);
          merged.recordHeap.remove(mergedLoc);
          merged.recordHeap.remove(otherLoc);
          merged.recordHeap.put(newRecord.getIntroductionLocation(), newRecord);
          merged.boundRecords.put(entry.getKey(), newRecord.getIntroductionLocation());
        }
      } else {
        merged.boundRecords.put(entry.getKey(), entry.getValue());
      }
    }
    for (Map.Entry<Integer, IRFlowExponential> entry : other.exponentialHeap.entrySet()) {
      if (merged.exponentialHeap.containsKey(entry.getKey())) {
        merged.exponentialHeap.get(entry.getKey()).merge(entry.getValue());
      } else {
        merged.exponentialHeap.put(entry.getKey(), entry.getValue().clone());
      }
    }
    for (Map.Entry<Integer, Integer> entry : other.boundExponentials.entrySet()) {
      if (merged.boundExponentials.containsKey(entry.getKey())) {
        merged.boundExponentials.put(
            entry.getKey(), merged.allocateExponential(Optional.empty()).getHeapLocation());
      } else {
        merged.boundExponentials.put(entry.getKey(), entry.getValue());
      }
    }
    for (int index : other.boundTypes.keySet()) {
      if (merged.boundTypes.containsKey(index)) {
        merged.bindType(index, merged.boundTypes.get(index).merge(other.boundTypes.get(index)));
      } else {
        merged.bindType(index, other.boundTypes.get(index));
      }
    }
    if (!this.pendingContinuations.equals(other.pendingContinuations)) {
      merged.pendingContinuations = Optional.empty();
    }
    return merged;
  }

  public IRFlowState clone() {
    IRFlowState clone = new IRFlowState();
    for (Map.Entry<IRFlowLocation, IRFlowRecord> entry : this.recordHeap.entrySet()) {
      clone.recordHeap.put(entry.getKey(), entry.getValue().clone());
    }
    clone.boundRecords = new HashMap<>(this.boundRecords);
    for (Map.Entry<Integer, IRFlowExponential> entry : this.exponentialHeap.entrySet()) {
      clone.exponentialHeap.put(entry.getKey(), entry.getValue().clone());
    }
    clone.boundExponentials = new HashMap<>(this.boundExponentials);
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

    for (Map.Entry<Integer, IRFlowLocation> boundEntry : boundRecords.entrySet()) {
      sb.append("record ").append(boundEntry.getKey());
      sb.append(" (@").append(boundEntry.getValue());
      sb.append("): ");
      sb.append(recordHeap.get(boundEntry.getValue()));
      sb.append("\n");
    }
    for (Map.Entry<Integer, IRFlowExponential> entry : exponentialHeap.entrySet()) {
      sb.append("exponential @").append(entry.getKey()).append(": ").append("bind=");
      boolean foundBinding = false;
      for (Map.Entry<Integer, Integer> boundEntry : boundExponentials.entrySet()) {
        if (boundEntry.getValue() == entry.getKey()) {
          if (foundBinding) {
            sb.append(",");
          }
          foundBinding = true;
          sb.append(boundEntry.getKey());
        }
      }
      if (!foundBinding) {
        sb.append("none");
      }

      sb.append(" ").append(entry.getValue()).append("\n");
    }
    for (Map.Entry<Integer, IRFlowType> entry : boundTypes.entrySet()) {
      sb.append("type ").append(entry.getKey()).append(": ").append(entry.getValue()).append("\n");
    }
    if (pendingContinuations.isEmpty()) {
      sb.append("pending: ?\n");
    } else {
      for (IRFlowContinuation pending : pendingContinuations.get()) {
        sb.append("pending: ").append(pending).append("\n");
      }
    }
    return sb.toString();
  }

  public Optional<Boolean> isValue(IRType type) {
    return isValue(type.valueRequisites());
  }

  public Optional<Boolean> isValue(IRType.ValueRequisites requisites) {
    if (requisites.mustBeValue()) {
      return Optional.of(true);
    } else if (requisites.canBeValue()) {
      boolean certainlyAValue = true;

      for (int t : requisites.getTypesWhichMustBeValues()) {
        Optional<IRType.ValueRequisites> req = boundType(t).getType().map(IRType::valueRequisites);
        if (req.isEmpty()) {
          certainlyAValue = false;
          continue;
        }

        Optional<Boolean> res = isValue(req.get());
        if (res.isEmpty()) {
          certainlyAValue = false;
        } else if (!res.get()) {
          return Optional.of(false);
        }
      }

      for (Map.Entry<Integer, Boolean> e : requisites.getRequiredTypePolarities().entrySet()) {
        Optional<Boolean> isPositive = boundType(e.getKey()).isPositive();
        if (isPositive.isEmpty()) {
          certainlyAValue = false;
        } else if (isPositive.get() != e.getValue()) {
          return Optional.of(false);
        }
      }

      return certainlyAValue ? Optional.of(true) : Optional.empty();
    } else {
      return Optional.of(false);
    }
  }

  public Optional<Integer> slotCount(IRType type, Optional<List<IRFlowSlot>> value) {
    SlotCounter counter = new SlotCounter();
    counter.value = value;
    type.accept(counter);
    return counter.count;
  }

  private class SlotCounter extends IRTypeVisitor {
    private Optional<List<IRFlowSlot>> value = Optional.empty();
    private Optional<Integer> count = Optional.empty();

    private Optional<Integer> recurse(IRType type, int offset) {
      return slotCount(type, value.map(l -> l.subList(offset, value.get().size())));
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException("Unsupported type: " + type);
    }

    @Override
    public void visit(IRCloseT type) {
      count = Optional.of(1);
    }

    @Override
    public void visit(IRSessionT type) {
      Optional<Boolean> isValue = isValue(type.getArg());
      if (isValue.isEmpty()) {
        return;
      }

      if (isValue.get()) {
        Optional<Integer> argSlots = recurse(type.getArg(), 0);
        if (argSlots.isPresent()) {
          Optional<Integer> contSlots = recurse(type.getCont(), argSlots.get());
          count = contSlots.map(slots -> slots + argSlots.get());
        }
      } else {
        count = recurse(type.getCont(), 1).map(slots -> slots + 1);
      }
    }

    @Override
    public void visit(IRVarT type) {
      if (boundTypes.containsKey(type.getType())) {
        Optional<IRType> t = boundTypes.get(type.getType()).getType();
        if (t.isPresent()) {
          count = recurse(t.get(), 0);
        }
      }
    }

    @Override
    public void visit(IRTagT type) {
      if (value.isEmpty() || value.get().isEmpty()) {
        return;
      }

      IRFlowSlot tag = value.get().get(0);
      if (!tag.isKnownTag()) {
        return;
      }

      IRType choice = type.getChoices().get(tag.getTag());
      count = recurse(choice, 1);
    }

    @Override
    public void visit(IRResetT type) {
      type.getCont().accept(this);
    }

    @Override
    public void visit(IRCellT type) {
      count = Optional.of(1);
    }

    @Override
    public void visit(IRTypeT type) {
      count = Optional.of(2);
    }

    @Override
    public void visit(IRStringT type) {
      count = Optional.of(1);
    }

    @Override
    public void visit(IRBoolT type) {
      count = Optional.of(1);
    }

    @Override
    public void visit(IRIntT type) {
      count = Optional.of(1);
    }

    @Override
    public void visit(IRExponentialT type) {
      count = Optional.of(1);
    }

    @Override
    public void visit(IRRecT type) {}
  }
}
