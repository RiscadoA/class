package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;

public class IRFlowSlot {
  private static enum Type {
    UNKNOWN,
    CLOSE,
    TAG,
    INTEGER,
    BOOL,
    STRING,
    RECORD,
    EXPONENTIAL,
    CELL,
    TYPE
  }

  private Type slotType = Type.UNKNOWN;
  private Optional<Integer> recordHeapLocation = Optional.empty();
  private Optional<Integer> exponentialHeapLocation = Optional.empty();
  private Optional<Integer> tag = Optional.empty();
  private Optional<IRFlowType> type = Optional.empty();

  public static IRFlowSlot unknown() {
    return new IRFlowSlot();
  }

  public static IRFlowSlot close() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.CLOSE;
    return slot;
  }

  public static IRFlowSlot tag(int tag) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.TAG;
    slot.tag = Optional.of(tag);
    return slot;
  }

  public static IRFlowSlot record(int heapLocation) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.RECORD;
    slot.recordHeapLocation = Optional.of(heapLocation);
    return slot;
  }

  public static IRFlowSlot exponential(int heapLocation) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.EXPONENTIAL;
    slot.exponentialHeapLocation = Optional.of(heapLocation);
    return slot;
  }

  public static IRFlowSlot cell() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.CELL;
    return slot;
  }

  public static IRFlowSlot integer() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.INTEGER;
    return slot;
  }

  public static IRFlowSlot bool() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.BOOL;
    return slot;
  }

  public static IRFlowSlot string() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.STRING;
    return slot;
  }

  public static IRFlowSlot type(IRFlowType type) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.TYPE;
    slot.type = Optional.of(type);
    return slot;
  }

  public boolean isKnownRecord() {
    return recordHeapLocation.isPresent();
  }

  public int getRecordHeapLocation() {
    return recordHeapLocation.orElseThrow(
        () -> new IllegalStateException("Slot does not hold a known record"));
  }

  public boolean isKnownTag() {
    return tag.isPresent();
  }

  public int getTag() {
    return tag.orElseThrow(() -> new IllegalStateException("Slot does not hold a known tag"));
  }

  public boolean isKnownExponential() {
    return exponentialHeapLocation.isPresent();
  }

  public int getExponentialHeapLocation() {
    return exponentialHeapLocation.orElseThrow(
        () -> new IllegalStateException("Slot does not hold a known exponential"));
  }

  public boolean isValue() {
    return slotType == Type.INTEGER
        || slotType == Type.BOOL
        || slotType == Type.STRING
        || slotType == Type.EXPONENTIAL;
  }

  public boolean isKnownType() {
    return type.isPresent();
  }

  public IRFlowType getType() {
    return type.orElseThrow(() -> new IllegalStateException("Slot does not hold a known type"));
  }

  public void markLost(IRFlowState state) {
    if (recordHeapLocation.isPresent()) {
      state.getHeapRecord(recordHeapLocation.get()).markTotallyUnknown(state);
    }
  }

  public IRFlowSlot merge(IRFlowSlot other) {
    if (this.slotType != other.slotType) {
      return unknown();
    }

    switch (this.slotType) {
      case TAG:
        return this.getTag() == other.getTag() ? this : unknown();
      case RECORD:
        return this.getRecordHeapLocation() == other.getRecordHeapLocation() ? this : unknown();
      case EXPONENTIAL:
        return this.getExponentialHeapLocation() == other.getExponentialHeapLocation()
            ? this
            : unknown();
      case TYPE:
        return type(this.getType().merge(other.getType()));
      default:
        return this;
    }
  }

  public IRFlowSlot clone() {
    return this;
  }

  @Override
  public String toString() {
    switch (slotType) {
      case UNKNOWN:
        return "?";
      case CLOSE:
        return "close";
      case TAG:
        return "tag(" + tag.get() + ")";
      case INTEGER:
        return "integer";
      case BOOL:
        return "boolean";
      case STRING:
        return "string";
      case RECORD:
        return "record(" + recordHeapLocation.get() + ")";
      case EXPONENTIAL:
        return "exponential(" + exponentialHeapLocation.get() + ")";
      case CELL:
        return "cell";
      case TYPE:
        return "type(" + type.get() + ")";
      default:
        throw new IllegalStateException("Unknown slot type: " + slotType);
    }
  }
}
