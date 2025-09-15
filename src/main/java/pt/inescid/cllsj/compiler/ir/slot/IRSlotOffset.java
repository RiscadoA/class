package pt.inescid.cllsj.compiler.ir.slot;

public class IRSlotOffset {
  public static final IRSlotOffset ZERO = new IRSlotOffset();

  private IRSlotTree past;
  private IRSlotTree future;

  public static IRSlotOffset of(IRSlotTree past, IRSlotTree future) {
    return new IRSlotOffset(past, future);
  }

  public static IRSlotOffset of(IRSlotSequence past, IRSlotTree future) {
    return new IRSlotOffset(IRSlotTree.of(past), future);
  }

  public static IRSlotOffset of(IRSlot past, IRSlotTree future) {
    return new IRSlotOffset(IRSlotTree.of(past), future);
  }

  private IRSlotOffset() {
    this.past = IRSlotTree.LEAF;
    this.future = IRSlotTree.LEAF;
  }

  private IRSlotOffset(IRSlotTree past, IRSlotTree future) {
    this.past = past;
    this.future = future;
  }

  public boolean isZero() {
    return past.isLeaf();
  }

  public IRSlotTree getPast() {
    return past;
  }

  public IRSlotTree getFuture() {
    return future;
  }

  public IRSlotOffset advance(IRSlot slot, IRSlotTree future) {
    return new IRSlotOffset(past.suffix(IRSlotTree.of(slot)), future);
  }

  public IRSlotOffset advance(IRSlotSequence slots, IRSlotTree future) {
    return new IRSlotOffset(past.suffix(IRSlotTree.of(slots)), future);
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
