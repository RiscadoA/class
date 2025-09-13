package pt.inescid.cllsj.compiler.ir.slot;

public class IRSlotStaticOffset {
  public static final IRSlotStaticOffset ZERO = new IRSlotStaticOffset();

  private IRSlotSequence past;
  private IRSlotCombinations future;

  public static IRSlotStaticOffset of(IRSlotSequence past, IRSlotCombinations future) {
    return new IRSlotStaticOffset(past, future);
  }

  public static IRSlotStaticOffset of(IRSlot past, IRSlotCombinations future) {
    return new IRSlotStaticOffset(IRSlotSequence.of(past), future);
  }

  private IRSlotStaticOffset() {
    this.past = IRSlotSequence.EMPTY;
    this.future = IRSlotCombinations.EMPTY;
  }

  private IRSlotStaticOffset(IRSlotSequence past, IRSlotCombinations future) {
    this.past = past;
    this.future = future;
  }

  public boolean isZero() {
    return past.size() == 0;
  }

  public IRSlotSequence getPast() {
    return past;
  }

  public IRSlotCombinations getFuture() {
    return future;
  }

  public IRSlotStaticOffset advance(IRSlot slot, IRSlotCombinations future) {
    return new IRSlotStaticOffset(past.suffix(slot), future);
  }

  public IRSlotStaticOffset advance(IRSlotSequence slots, IRSlotCombinations future) {
    return new IRSlotStaticOffset(past.suffix(slots), future);
  }

  public IRSlotStaticOffset advance(IRSlotStaticOffset offset) {
    return new IRSlotStaticOffset(past.suffix(offset.past), offset.future);
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
