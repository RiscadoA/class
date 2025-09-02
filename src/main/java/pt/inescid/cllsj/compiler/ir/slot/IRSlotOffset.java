package pt.inescid.cllsj.compiler.ir.slot;

import java.util.Optional;

public class IRSlotOffset {
  public static final IRSlotOffset ZERO = new IRSlotOffset();

  private IRSlotSequence past;
  private Optional<IRSlot> alignTo;

  public IRSlotOffset() {
    this.past = IRSlotSequence.EMPTY;
    this.alignTo = Optional.empty();
  }

  public IRSlotOffset(IRSlotSequence past, IRSlot alignTo) {
    this.past = past;
    this.alignTo = Optional.of(alignTo);
  }

  public IRSlotOffset(IRSlotSequence past, Optional<IRSlot> alignTo) {
    this.past = past;
    this.alignTo = alignTo;
  }

  public boolean isZero() {
    return this.equals(ZERO);
  }

  public IRSlotSequence getPast() {
    return past;
  }

  public Optional<IRSlot> getAlignTo() {
    return alignTo;
  }

  public IRSlotOffset advance(IRSlot slot, IRSlot alignTo) {
    return new IRSlotOffset(past.suffix(slot), alignTo);
  }

  public IRSlotOffset advance(IRSlotSequence slots, IRSlot alignTo) {
    return new IRSlotOffset(past.suffix(slots), alignTo);
  }

  public IRSlotOffset advance(IRSlotOffset offset) {
    return new IRSlotOffset(past.suffix(offset.past), offset.alignTo.or(() -> this.alignTo));
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("[");
    if (past.size() > 0) {
      sb.append(past.get(0));
    }
    for (int i = 1; i < past.size(); i++) {
      sb.append("; ").append(past.get(i));
    }
    if (alignTo.isPresent()) {
      sb.append("|").append(alignTo.get());
    }
    sb.append("]");
    return sb.toString();
  }

  @Override
  public boolean equals(Object obj) {
    return toString().equals(obj.toString());
  }

  @Override
  public int hashCode() {
    return toString().hashCode();
  }
}
