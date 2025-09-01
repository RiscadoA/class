package pt.inescid.cllsj.compiler.ir.old.flow;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import pt.inescid.cllsj.compiler.old.IRAnalyzer;

public class IRFlowRecord {
  private IRFlowLocation introductionLocation;
  private Optional<IRFlowContinuation> continuation = Optional.empty();

  private List<IRFlowSlot> slots = new ArrayList<>();
  private Optional<Integer> nextSlotIndex = Optional.empty();
  private boolean slotsKnown = false;

  public IRFlowRecord(IRFlowLocation introductionLocation) {
    this.introductionLocation = introductionLocation;
  }

  public void doNewSession(Optional<IRFlowContinuation> continuation) {
    this.slots.clear();
    this.slotsKnown = true;
    this.nextSlotIndex = Optional.of(0);
    this.continuation = continuation;
  }

  // Gets the next slot that will be popped, if it is known.
  public Optional<IRFlowSlot> peek() {
    if (!slotsKnown) {
      return Optional.empty();
    }
    return Optional.of(slots.getFirst());
  }

  // Performs a read on the record.
  // If the slot being read is known, it is returned.
  public IRFlowSlot doPop(IRFlowLocation location) {
    if (!slotsKnown) {
      return IRFlowSlot.unknown(location);
    }

    return slots.removeFirst();
  }

  // Performs a write on the record.
  public void doPush(IRAnalyzer analyzer, IRFlowState state, IRFlowSlot slot) {
    if (!slotsKnown) {
      slot.markLost(analyzer, state);
      return;
    }

    slots.add(slot);
    nextSlotIndex = nextSlotIndex.map(i -> i + 1);
  }

  public void doPushUnfold() {
    slots.clear();
    slotsKnown = true;
    nextSlotIndex = Optional.empty();
  }

  public void doPopUnfold() {
    if (slotsKnown) {
      slots.clear();
      nextSlotIndex = Optional.empty();
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
  public void markContinuationUnknown(IRAnalyzer analyzer, IRFlowState state) {
    if (this.continuation.isPresent()) {
      state.pushPendingContinuation(analyzer, this.continuation.get());
    }
    this.continuation = Optional.empty();
  }

  // Mark the record's slots as completely unknown.
  public void markSlotsUnknown(IRAnalyzer analyzer, IRFlowState state) {
    this.slotsKnown = false;
    for (IRFlowSlot slot : slots) {
      slot.markLost(analyzer, state);
    }
    this.slots.clear();
    this.nextSlotIndex = Optional.empty();
  }

  public void markTotallyUnknown(IRAnalyzer analyzer, IRFlowState state) {
    markContinuationUnknown(analyzer, state);
    markSlotsUnknown(analyzer, state);
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

  public Optional<Integer> getNextSlotIndex() {
    if (!slotsKnown) {
      return Optional.empty();
    }
    return nextSlotIndex;
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
    if (merged.continuation.isPresent() && other.continuation.isPresent()) {
      merged.continuation = merged.continuation.get().merge(other.continuation.get());
    } else {
      merged.continuation = Optional.empty();
    }
    if (merged.introductionLocation != other.introductionLocation) {
      merged.introductionLocation = IRFlowLocation.unknown();
    }
    if (!merged.slotsKnown || !other.slotsKnown || merged.slots.size() != other.slots.size()) {
      merged.slots.clear();
      merged.slotsKnown = false;
    }
    if (!merged.nextSlotIndex.equals(other.nextSlotIndex)) {
      merged.nextSlotIndex = Optional.empty();
    }
    for (int i = 0; i < merged.slots.size(); ++i) {
      merged.slots.set(i, merged.slots.get(i).merge(other.slots.get(i)));
    }
    return merged;
  }

  public IRFlowRecord clone() {
    IRFlowRecord clone = new IRFlowRecord(introductionLocation);
    clone.continuation = this.continuation;
    clone.slots.addAll(this.slots);
    clone.slotsKnown = this.slotsKnown;
    clone.nextSlotIndex = this.nextSlotIndex;
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
