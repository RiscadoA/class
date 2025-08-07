package pt.inescid.cllsj.compiler.ir.flow;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;

public class IRFlowState {
  private Map<Integer, IRFlowRecord> records = new HashMap<>();
  private Queue<String> pendingContinuations = new LinkedList<>();

  public IRFlowRecord record(int id) {
    return records.computeIfAbsent(id, k -> new IRFlowRecord(id));
  }

  public void bindRecord(int id, IRFlowRecord record) {
    records.put(id, record);
  }

  public void unbindRecord(int id) {
    records.remove(id);
  }

  public void pushPendingContinuation(String continuation) {
    pendingContinuations.add(continuation);
  }

  public Optional<String> popPendingContinuation() {
    if (pendingContinuations.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(pendingContinuations.poll());
  }

  public List<String> getPendingContinuations() {
    return new LinkedList<>(pendingContinuations);
  }

  public IRFlowState clone() {
    IRFlowState clone = new IRFlowState();
    for (Map.Entry<Integer, IRFlowRecord> entry : this.records.entrySet()) {
      clone.records.put(entry.getKey(), entry.getValue().clone());
    }
    clone.pendingContinuations.addAll(this.pendingContinuations);
    return clone;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (Map.Entry<Integer, IRFlowRecord> entry : records.entrySet()) {
      sb.append("record ")
          .append(entry.getKey())
          .append(": ")
          .append(entry.getValue())
          .append("\n");
    }
    sb.append("pending_continuations: ");
    if (pendingContinuations.isEmpty()) {
      sb.append("none");
    } else {
      sb.append(String.join(", ", pendingContinuations));
    }
    return sb.toString();
  }
}
