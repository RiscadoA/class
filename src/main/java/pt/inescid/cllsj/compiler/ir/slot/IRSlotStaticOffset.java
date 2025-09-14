package pt.inescid.cllsj.compiler.ir.slot;

public class IRSlotStaticOffset {
  public static final IRSlotStaticOffset ZERO = new IRSlotStaticOffset();

  private IRSlotCombinations past;
  private IRSlotCombinations future;

  public static IRSlotStaticOffset of(IRSlotCombinations past, IRSlotCombinations future) {
    return new IRSlotStaticOffset(past, future);
  }

  public static IRSlotStaticOffset of(IRSlotSequence past, IRSlotCombinations future) {
    return new IRSlotStaticOffset(IRSlotCombinations.of(past), future);
  }

  public static IRSlotStaticOffset of(IRSlot past, IRSlotCombinations future) {
    return new IRSlotStaticOffset(IRSlotCombinations.of(past), future);
  }

  private IRSlotStaticOffset() {
    this.past = IRSlotCombinations.EMPTY;
    this.future = IRSlotCombinations.EMPTY;
  }

  private IRSlotStaticOffset(IRSlotCombinations past, IRSlotCombinations future) {
    this.past = past;
    this.future = future;
  }

  public boolean isZero() {
    return past.size() == 0;
  }

  public IRSlotCombinations getPast() {
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

  public IRSlotStaticOffset advance(IRSlotCombinations slots, IRSlotCombinations future) {
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
