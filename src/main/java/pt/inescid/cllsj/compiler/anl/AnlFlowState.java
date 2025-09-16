package pt.inescid.cllsj.compiler.anl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Stream;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class AnlFlowState {
  private Map<IRSessionId, AnlSessionState> sessions = new HashMap<>();
  private Map<IRLocalDataId, AnlLocalDataState> localData = new HashMap<>();

  // Local data that has been 'lost', i.e., possibly passed to some other process
  // These sections are cleared when some write to an unknown location occurs, or
  // when control flow passes to an unknown location
  private Set<IRLocalDataId> passedLocalData = new HashSet<>();

  // Continuations that still need to be processed
  private Stack<AnlFlowContinuation> pendingContinuations = new Stack<>();

  public AnlSessionState session(IRSessionId id) {
    return sessions.computeIfAbsent(id, k -> new AnlSessionState());
  }

  public AnlLocalDataState localData(IRLocalDataId id) {
    return localData.computeIfAbsent(id, k -> new AnlLocalDataState());
  }

  public AnlSlot read(IRDataLocation location) {
    return read(location, Optional.empty()).findFirst().orElse(AnlSlot.UNKNOWN);
  }

  public Stream<AnlSlot> read(IRDataLocation location, IRSlotTree size) {
    return read(location, Optional.of(size));
  }

  public Stream<AnlSlot> read(IRDataLocation location, Optional<IRSlotTree> size) {
    if (location.isLocal()) {
      return localData(location.getLocalDataId()).read(location.getOffset().getPast(), size);
    } else if (location.isRemote()) {
      AnlSessionState session = session(location.getSessionId());
      if (session.data.isPresent()) {
        // We're reading from a known location
        return read(session.data.get().advance(location.getOffset()), size);
      }
    }

    if (size.isEmpty()) {
      return Stream.of(AnlSlot.UNKNOWN);
    } else {
      int count = AnlSlot.count(size.get());
      return Stream.iterate(0, i -> i + 1).limit(count).map(i -> AnlSlot.UNKNOWN);
    }
  }

  public void write(Analyzer analyzer, IRDataLocation location, AnlSlot slot) {
    write(analyzer, location, Stream.of(slot));
  }

  public void write(Analyzer analyzer, IRDataLocation location, Stream<AnlSlot> slots) {
    if (location.isLocal()) {
      localData(location.getLocalDataId()).write(location.getOffset().getPast(), slots);
      return;
    } else if (location.isRemote()) {
      AnlSessionState session = session(location.getSessionId());
      if (session.data.isPresent()) {
        // We're writing to a known location
        write(analyzer, session.data.get().advance(location.getOffset()), slots);
        return;
      }
    }

    // We're writing to an unknown location
    handleUnknownWrites(analyzer);
    slots.forEach(s -> s.markAsUnknown(analyzer, this));
  }

  // Called on a data location when a reference to it is lost
  // Basically means we can no longer assume that it won't change when writing to unknown spots
  public void markDataAsUnknown(IRDataLocation loc) {
    if (loc.isLocal()) {
      passedLocalData.add(loc.getLocalDataId());
    } else if (loc.isRemote()) {
      AnlSessionState session = session(loc.getSessionId());
      if (session.data.isPresent()) {
        IRDataLocation dataLoc = session.data.get().advance(loc.getOffset());
        markDataAsUnknown(dataLoc);
      }
    }
  }

  public void handleUnknownWrites(Analyzer analyzer) {
    // If an unknown write occurs, we must mark all local data that may be referenced
    // by unknown locations as unknown
    for (IRLocalDataId id : passedLocalData) {
      if (localData.containsKey(id)) {
        localData.get(id).markAsUnknown(analyzer, this);
      }
    }
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
      if (merged.sessions.containsKey(entry.getKey())) {
        merged.sessions.put(
            entry.getKey(), merged.sessions.get(entry.getKey()).merge(entry.getValue()));
      } else {
        merged.sessions.put(entry.getKey(), entry.getValue().clone());
      }
    }
    for (Map.Entry<IRLocalDataId, AnlLocalDataState> entry : other.localData.entrySet()) {
      if (merged.localData.containsKey(entry.getKey())) {
        merged.localData.put(
            entry.getKey(), merged.localData.get(entry.getKey()).merge(entry.getValue()));
      } else {
        merged.localData.put(entry.getKey(), entry.getValue().clone());
      }
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
    for (Map.Entry<IRLocalDataId, AnlLocalDataState> entry : this.localData.entrySet()) {
      clone.localData.put(entry.getKey(), entry.getValue().clone());
    }
    clone.pendingContinuations.addAll(this.pendingContinuations);
    return clone;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();

    for (Map.Entry<IRSessionId, AnlSessionState> entry : sessions.entrySet()) {
      sb.append(entry.getKey()).append(": ").append(entry.getValue()).append("\n");
    }
    for (AnlFlowContinuation pending : pendingContinuations) {
      sb.append("pending: ").append(pending).append("\n");
    }
    return sb.toString();
  }
}
