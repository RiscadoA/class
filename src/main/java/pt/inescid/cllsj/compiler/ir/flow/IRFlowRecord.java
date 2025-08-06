package pt.inescid.cllsj.compiler.ir.flow;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.function.Consumer;

public class IRFlowRecord {
  private Optional<String> continuation = Optional.empty();
  private int index;

  private List<IRFlowSlot> slots = new ArrayList<>();
  private int readCursor = 0;
  private boolean slotsKnown = false;

  public IRFlowRecord(int index) {
    this.index = index;
  }

  public void doNewSession(Optional<String> continuation) {
    this.slots.clear();
    this.readCursor = 0;
    this.slotsKnown = true;
    this.continuation = continuation;
  }

  // Performs a read on the record.
  // If the slot being read is known, it is returned.
  public Optional<IRFlowSlot> doPop() {
    if (!slotsKnown) {
      return Optional.empty();
    }

    return Optional.of(slots.get(readCursor++));
  }

  // Performs a write on the record.
  public void doPush(IRFlowSlot slot) {
    if (!slotsKnown) {
      return;
    }

    slots.add(slot);
  }

  // Sets a new continuation and returns the old one.
  public Optional<String> doFlip(String continuation) {
    Optional<String> oldContinuation = this.continuation;
    this.continuation = Optional.of(continuation);
    return oldContinuation;
  }

  // Sets the continuation to unknown and returns the old continuation.
  public Optional<String> doReturn() {
    Optional<String> oldContinuation = this.continuation;
    this.continuation = Optional.empty();
    return oldContinuation;
  }

  // Returns the current continuation, if known.
  public Optional<String> getContinuation() {
    return continuation;
  }

  // Mark the record's continuation as unknown.
  public void markContinuationUnknown() {
    this.continuation = Optional.empty();
  }

  // Mark the record's slots as completely unknown.
  public void markSlotsUnknown(IRFlowState state) {
    this.slotsKnown = false;
    for (int i = readCursor; i < slots.size(); i++) {
      IRFlowSlot slot = slots.get(i);
      if (slot.isKnownRecord()) {
        Optional<String> cont = slot.getRecord().getContinuation();
        if (cont.isPresent()) {
          state.pushPendingContinuation(cont.get());
        }
        slot.getRecord().markContinuationUnknown();
        slot.getRecord().markSlotsUnknown(state);
      }
    }
    this.slots.clear();
  }

  public void markTotallyUnknown(IRFlowState state) {
    markContinuationUnknown();
    markSlotsUnknown(state);
  }

  public void doPopAll(Consumer<IRFlowSlot> consumer) {
    if (!slotsKnown) {
      return;
    }

    while (readCursor < slots.size()) {
      consumer.accept(slots.get(readCursor++));
    }
  }

  public boolean slotsAreKnown() {
    return slotsKnown;
  }

  public int getIndex() {
    return index;
  }

  public IRFlowRecord clone() {
    IRFlowRecord clone = new IRFlowRecord(index);
    clone.continuation = this.continuation;
    clone.slots.addAll(this.slots);
    return clone;
  }
}
