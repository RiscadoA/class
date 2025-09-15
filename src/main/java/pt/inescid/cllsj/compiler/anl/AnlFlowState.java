package pt.inescid.cllsj.compiler.anl;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class AnlFlowState {
  private Map<IRSessionId, AnlSessionState> sessions = new HashMap<>();
  private Stack<AnlFlowContinuation> pendingContinuations = new Stack<>();

  public AnlSessionState session(IRSessionId id) {
    return sessions.computeIfAbsent(id, k -> new AnlSessionState());
  }

  public void pushPendingContinuation(Analyzer analyzer, AnlFlowContinuation cont) {
    analyzer.getFlow().addDetached(analyzer.getFlow(cont.getLocation()));
    pendingContinuations.push(cont);
  }

  public void pushPendingContinuation(
      Analyzer analyzer, IRCodeLocation cLocation, AnlFlowLocation fLocation) {
    pushPendingContinuation(analyzer, new AnlFlowContinuation(cLocation, fLocation));
  }

  public Optional<AnlFlowContinuation> popPendingContinuation() {
    if (pendingContinuations.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(pendingContinuations.pop());
  }

  public Stack<AnlFlowContinuation> getPendingContinuations() {
    return pendingContinuations;
  }

  public AnlFlowState merge(Analyzer analyzer, AnlFlowLocation location, AnlFlowState other) {
    if (this == other) {
      return this;
    }

    AnlFlowState merged = this.clone();
    for (Map.Entry<IRSessionId, AnlSessionState> entry : other.sessions.entrySet()) {
      merged.sessions.put(entry.getKey(), session(entry.getKey()).merge(entry.getValue()));
    }
    if (!this.pendingContinuations.equals(other.pendingContinuations)) {
      merged.pendingContinuations.clear();
    }
    return merged;
  }

  public AnlFlowState clone() {
    AnlFlowState clone = new AnlFlowState();
    for (Map.Entry<IRSessionId, AnlSessionState> entry : this.sessions.entrySet()) {
      clone.sessions.put(entry.getKey(), entry.getValue().clone());
    }
    clone.pendingContinuations.addAll(this.pendingContinuations);
    return clone;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();

    for (Map.Entry<IRSessionId, AnlSessionState> entry : sessions.entrySet()) {
      sb.append("session ")
          .append(entry.getKey())
          .append(": ")
          .append(entry.getValue())
          .append("\n");
    }

    if (pendingContinuations.isEmpty()) {
      sb.append("pending: ?\n");
    } else {
      for (AnlFlowContinuation pending : pendingContinuations.get()) {
        sb.append("pending: ").append(pending).append("\n");
      }
    }
    return sb.toString();
  }
}
