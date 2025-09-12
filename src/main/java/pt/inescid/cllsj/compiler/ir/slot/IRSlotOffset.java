package pt.inescid.cllsj.compiler.ir.slot;

public class IRSlotOffset {
  public static final IRSlotOffset ZERO = new IRSlotOffset();

  private IRSlotCombinations past;
  private IRSlotCombinations future;

  public static IRSlotOffset of(IRSlotSequence past, IRSlotCombinations future) {
    return new IRSlotOffset(past, future);
  }

  public static IRSlotOffset of(IRSlot past, IRSlotCombinations future) {
    return new IRSlotOffset(IRSlotSequence.of(past), future);
  }

  public IRSlotOffset() {
    this.past = IRSlotCombinations.EMPTY;
    this.future = IRSlotCombinations.EMPTY;
  }

  public IRSlotOffset(IRSlotSequence past, IRSlotCombinations future) {
    this.past = IRSlotCombinations.of(past);
    this.future = future;
  }

  public IRSlotOffset(IRSlotCombinations past, IRSlotCombinations future) {
    this.past = past;
    this.future = future;
  }

  public boolean isZero() {
    return past.isEmpty();
  }

  public IRSlotCombinations getPast() {
    return past;
  }

  public IRSlotCombinations getFuture() {
    return future;
  }

  public IRSlotOffset advance(IRSlot slot, IRSlotCombinations future) {
    return new IRSlotOffset(past.suffix(slot), future);
  }

  public IRSlotOffset advance(IRSlotSequence slots, IRSlotCombinations future) {
    return new IRSlotOffset(past.suffix(slots), future);
  }

  public IRSlotOffset advance(IRSlotOffset offset) {
    return new IRSlotOffset(past.suffix(offset.past), offset.future);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append(past);
    sb.append("~");
    sb.append(future);
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
