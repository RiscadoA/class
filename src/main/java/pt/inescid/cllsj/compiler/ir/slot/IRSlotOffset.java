package pt.inescid.cllsj.compiler.ir.slot;

import java.util.Optional;

public class IRSlotOffset {
  public static final IRSlotOffset ZERO = new IRSlotOffset();

  private IRSlotTree past;
  private Optional<IRSlot> alignTo;

  public IRSlotOffset() {
    this.past = IRSlotTree.LEAF;
    this.alignTo = Optional.empty();
  }

  public IRSlotOffset(IRSlotTree past, IRSlot alignTo) {
    this.past = past;
    this.alignTo = Optional.of(alignTo);
  }

  public IRSlotOffset(IRSlotTree past, Optional<IRSlot> alignTo) {
    this.past = past;
    this.alignTo = alignTo;
  }

  public boolean isZero() {
    return this.equals(ZERO);
  }

  public IRSlotTree getPast() {
    return past;
  }

  public Optional<IRSlot> getAlignTo() {
    return alignTo;
  }

  public IRSlotOffset advance(IRSlot slot, IRSlot alignTo) {
    return new IRSlotOffset(past.suffix(IRSlotTree.of(slot)), alignTo);
  }

  public IRSlotOffset advance(IRSlotSequence slots, IRSlot alignTo) {
    return new IRSlotOffset(past.suffix(IRSlotTree.of(slots)), alignTo);
  }

  public IRSlotOffset advance(IRSlotOffset offset) {
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
