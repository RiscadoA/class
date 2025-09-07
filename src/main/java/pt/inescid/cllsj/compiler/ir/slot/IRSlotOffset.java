package pt.inescid.cllsj.compiler.ir.slot;

import java.util.Optional;

public class IRSlotOffset {
  public static final IRSlotOffset ZERO = new IRSlotOffset();

  private IRSlotCombinations past;
  private Optional<IRSlot> alignTo;

  public static IRSlotOffset of(IRSlotSequence past, IRSlot alignTo) {
    return new IRSlotOffset(past, alignTo);
  }

  public static IRSlotOffset of(IRSlot past, IRSlot alignTo) {
    return new IRSlotOffset(IRSlotSequence.of(past), alignTo);
  }

  public IRSlotOffset() {
    this.past = IRSlotCombinations.EMPTY;
    this.alignTo = Optional.empty();
  }

  public IRSlotOffset(IRSlotSequence past, IRSlot alignTo) {
    this.past = IRSlotCombinations.of(past);
    this.alignTo = Optional.of(alignTo);
  }

  public IRSlotOffset(IRSlotSequence past, Optional<IRSlot> alignTo) {
    this.past = IRSlotCombinations.of(past);
    this.alignTo = alignTo;
  }

  public IRSlotOffset(IRSlotCombinations past, IRSlot alignTo) {
    this.past = past;
    this.alignTo = Optional.of(alignTo);
  }

  public IRSlotOffset(IRSlotCombinations past, Optional<IRSlot> alignTo) {
    this.past = past;
    this.alignTo = alignTo;
  }

  public boolean isZero() {
    return past.isEmpty();
  }

  public IRSlotCombinations getPast() {
    return past;
  }

  public Optional<IRSlot> getAlignTo() {
    return alignTo;
  }

  public IRSlotOffset advance(IRSlot slot, IRSlot alignTo) {
    return new IRSlotOffset(past.suffix(slot), alignTo);
  }

  public IRSlotOffset advance(IRSlotSequence slots, IRSlot alignTo) {
    if (slots.size() == 0) {
      return this;
    } else {
      return new IRSlotOffset(past.suffix(slots), alignTo);
    }
  }

  public IRSlotOffset advance(IRSlotOffset offset) {
    if (offset.getPast().size() == 0) {
      return this;
    }
    return new IRSlotOffset(past.suffix(offset.past), offset.alignTo.or(() -> this.alignTo));
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(past);
    if (alignTo.isPresent()) {
      sb.append("~");
      sb.append(alignTo.get());
    }
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
