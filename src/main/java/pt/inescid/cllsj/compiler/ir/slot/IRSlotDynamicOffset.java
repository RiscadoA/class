package pt.inescid.cllsj.compiler.ir.slot;

public class IRSlotDynamicOffset {
  public static final IRSlotDynamicOffset ZERO = new IRSlotDynamicOffset();

  private IRSlotTree past;
  private IRSlotCombinations future;

  public static IRSlotDynamicOffset of(IRSlotTree past, IRSlotCombinations future) {
    return new IRSlotDynamicOffset(past, future);
  }

  public static IRSlotDynamicOffset of(IRSlotSequence past, IRSlotCombinations future) {
    return new IRSlotDynamicOffset(IRSlotTree.of(past), future);
  }

  public static IRSlotDynamicOffset of(IRSlot past, IRSlotCombinations future) {
    return new IRSlotDynamicOffset(IRSlotTree.of(past), future);
  }

  public static IRSlotDynamicOffset of(IRSlotStaticOffset offset) {
    return IRSlotDynamicOffset.of(offset.getPast(), offset.getFuture());
  }

  private IRSlotDynamicOffset() {
    this.past = IRSlotTree.LEAF;
    this.future = IRSlotCombinations.EMPTY;
  }

  private IRSlotDynamicOffset(IRSlotTree past, IRSlotCombinations future) {
    this.past = past;
    this.future = future;
  }

  public boolean isZero() {
    return past.isLeaf();
  }

  public IRSlotTree getPast() {
    return past;
  }

  public IRSlotCombinations getFuture() {
    return future;
  }

  public IRSlotDynamicOffset advance(IRSlot slot, IRSlotCombinations future) {
    return new IRSlotDynamicOffset(past.suffix(IRSlotTree.of(slot)), future);
  }

  public IRSlotDynamicOffset advance(IRSlotSequence slots, IRSlotCombinations future) {
    return new IRSlotDynamicOffset(past.suffix(IRSlotTree.of(slots)), future);
  }

  public IRSlotDynamicOffset advance(IRSlotDynamicOffset offset) {
    return new IRSlotDynamicOffset(past.suffix(offset.past), offset.future);
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
