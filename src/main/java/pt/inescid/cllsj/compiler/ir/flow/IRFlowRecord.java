package pt.inescid.cllsj.compiler.ir.flow;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

public class IRFlowRecord {
  private IRFlowLocation introductionLocation;
  private Optional<IRFlowContinuation> continuation = Optional.empty();

  private List<IRFlowSlot> slots = new ArrayList<>();
  private boolean slotsKnown = false;

  public IRFlowRecord(IRFlowLocation introductionLocation) {
    this.introductionLocation = introductionLocation;
  }

  public void doNewSession(Optional<IRFlowContinuation> continuation) {
    this.slots.clear();
    this.slotsKnown = true;
    this.continuation = continuation;
  }

  // Performs a read on the record.
  // If the slot being read is known, it is returned.
  public IRFlowSlot doPop() {
    if (!slotsKnown) {
      return IRFlowSlot.unknown();
    }

    return slots.removeFirst();
  }

  // Performs a write on the record.
  public void doPush(IRFlowState state, IRFlowSlot slot) {
    if (!slotsKnown) {
      slot.markLost(state);
      return;
    }

    slots.add(slot);
  }

  public void doPushUnfold() {
    slots.clear();
    slotsKnown = true;
  }

  public void doPopUnfold() {
    if (slotsKnown) {
      slots.clear();
    }
  }

  // Sets a new continuation and returns the old one.
  public Optional<IRFlowContinuation> doFlip(IRFlowContinuation continuation) {
    return doFlip(Optional.of(continuation));
  }

  // Sets the continuation to unknown and returns the old continuation.
  public Optional<IRFlowContinuation> doReturn() {
    return doFlip(Optional.empty());
  }

  public Optional<IRFlowContinuation> doFlip(Optional<IRFlowContinuation> continuation) {
    Optional<IRFlowContinuation> oldContinuation = this.continuation;
    this.continuation = continuation;
    return oldContinuation;
  }

  // Returns the current continuation, if known.
  public Optional<IRFlowContinuation> getContinuation() {
    return continuation;
  }

  // Mark the record's continuation as unknown.
  public void markContinuationUnknown(IRFlowState state) {
    if (this.continuation.isPresent()) {
      state.pushPendingContinuation(this.continuation.get());
    }
    this.continuation = Optional.empty();
  }

  // Mark the record's slots as completely unknown.
  public void markSlotsUnknown(IRFlowState state) {
    this.slotsKnown = false;
    for (IRFlowSlot slot : slots) {
      slot.markLost(state);
    }
    this.slots.clear();
  }

  public void markTotallyUnknown(IRFlowState state) {
    markContinuationUnknown(state);
    markSlotsUnknown(state);
  }

  public void doPopAll(Consumer<IRFlowSlot> consumer) {
    if (!slotsKnown) {
      return;
    }

    while (!slots.isEmpty()) {
      consumer.accept(slots.removeFirst());
    }
  }

  public boolean slotsAreKnown() {
    return slotsKnown;
  }

  public Optional<Integer> getSlotCount() {
    if (!slotsKnown) {
      return Optional.empty();
    }
    return Optional.of(slots.size());
  }

  public Optional<List<IRFlowSlot>> getSlots() {
    if (!slotsKnown) {
      return Optional.empty();
    }
    return Optional.of(slots);
  }

  public IRFlowLocation getIntroductionLocation() {
    return introductionLocation;
  }

  public IRFlowRecord merge(IRFlowRecord other) {
    IRFlowRecord merged = this.clone();
    if (merged.continuation.isEmpty()
        || other.continuation.isEmpty()
        || !merged.continuation.get().equals(other.continuation.get())) {
      merged.continuation = Optional.empty();
    }
    if (merged.introductionLocation != other.introductionLocation) {
      throw new IllegalArgumentException(
          "Cannot merge records with different introduction locations: "
              + merged.introductionLocation
              + " != "
              + other.introductionLocation);
    }
    if (!merged.slotsKnown || !other.slotsKnown || merged.slots.size() != other.slots.size()) {
      merged.slots.clear();
      merged.slotsKnown = false;
    }
    for (int i = 0; i < merged.slots.size(); ++i) {
      merged.slots.set(i, merged.slots.get(i).merge(other.slots.get(i)));
    }
    return merged;
  }

  public IRFlowRecord clone() {
    IRFlowRecord clone = new IRFlowRecord(introductionLocation);
    clone.continuation = this.continuation;
    for (IRFlowSlot slot : this.slots) {
      clone.slots.add(slot.clone());
    }
    clone.slotsKnown = this.slotsKnown;
    return clone;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("cont=");
    if (continuation.isEmpty()) {
      sb.append("?");
    } else {
      sb.append(continuation.get());
    }
    if (slotsKnown) {
      sb.append(" slots=[");
      for (int i = 0; i < slots.size(); ++i) {
        sb.append(slots.get(i).toString());
        if (i < slots.size() - 1) {
          sb.append(", ");
        }
      }
      sb.append("]");
    } else {
      sb.append(" slots=?");
    }
    return sb.toString();
  }
}
