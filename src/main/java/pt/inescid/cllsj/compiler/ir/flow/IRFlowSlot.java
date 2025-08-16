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
  private Optional<IRFlowLocation> recordIntroductionLocation = Optional.empty();
  private Optional<Integer> exponentialHeapLocation = Optional.empty();
  private Optional<Integer> tag = Optional.empty();
  private Optional<IRFlowType> type = Optional.empty();

  // Instruction which produced this slot.
  private Optional<IRFlowLocation> producer;

  private IRFlowSlot(IRFlowLocation producer) {
    this.producer = Optional.of(producer);
  }

  private IRFlowSlot() {
    this.producer = Optional.empty();
  }

  public static IRFlowSlot unknown(IRFlowLocation producer) {
    return new IRFlowSlot(producer);
  }

  public static IRFlowSlot close(IRFlowLocation producer) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.CLOSE;
    return slot;
  }

  public static IRFlowSlot tag(IRFlowLocation producer, int tag) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.TAG;
    slot.tag = Optional.of(tag);
    return slot;
  }

  public static IRFlowSlot record(IRFlowLocation producer, IRFlowLocation introductionLocation) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.RECORD;
    slot.recordIntroductionLocation = Optional.of(introductionLocation);
    return slot;
  }

  public static IRFlowSlot exponential(IRFlowLocation producer, int heapLocation) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.EXPONENTIAL;
    slot.exponentialHeapLocation = Optional.of(heapLocation);
    return slot;
  }

  public static IRFlowSlot cell(IRFlowLocation producer) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.CELL;
    return slot;
  }

  public static IRFlowSlot integer(IRFlowLocation producer) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.INTEGER;
    return slot;
  }

  public static IRFlowSlot bool(IRFlowLocation producer) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.BOOL;
    return slot;
  }

  public static IRFlowSlot string(IRFlowLocation producer) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.STRING;
    return slot;
  }

  public static IRFlowSlot type(IRFlowLocation producer, IRFlowType type) {
    IRFlowSlot slot = new IRFlowSlot(producer);
    slot.slotType = Type.TYPE;
    slot.type = Optional.of(type);
    return slot;
  }

  public boolean isKnownRecord() {
    return recordIntroductionLocation.isPresent();
  }

  public IRFlowLocation getRecordIntroductionLocation() {
    return recordIntroductionLocation.orElseThrow(
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
    if (recordIntroductionLocation.isPresent()) {
      state.getHeapRecord(recordIntroductionLocation.get()).markTotallyUnknown(state);
    }
  }

  public IRFlowSlot merge(IRFlowSlot other) {
    IRFlowSlot merged;
    if (producer.isEmpty() || !producer.equals(other.producer)) {
      merged = new IRFlowSlot();
    } else {
      merged = new IRFlowSlot(producer.get());
    }

    if (this.slotType != other.slotType) {
      return merged;
    }
    merged.slotType = this.slotType;

    switch (merged.slotType) {
      case TAG:
        merged.tag = this.tag.equals(other.tag) ? this.tag : Optional.empty();
        break;
      case RECORD:
        this.recordIntroductionLocation = this.recordIntroductionLocation.equals(other.recordIntroductionLocation)
            ? this.recordIntroductionLocation
            : Optional.empty();
      case EXPONENTIAL:
        merged.exponentialHeapLocation = this.exponentialHeapLocation.equals(other.exponentialHeapLocation)
            ? this.exponentialHeapLocation
            : Optional.empty();
        break;
      case TYPE:
        merged.type = this.type.equals(other.type) ? this.type : Optional.empty();
        break;
      default:
        break;
    }

    return merged;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    switch (slotType) {
      case UNKNOWN:
        sb.append("?");
        break;
      case CLOSE:
        sb.append("close");
      case TAG:
        sb.append("tag(").append(tag.get()).append(")");
        break;
      case INTEGER:
        sb.append("int");
        break;
      case BOOL:
        sb.append("bool");
        break;
      case STRING:
        sb.append("string");
        break;
      case RECORD:
        sb.append("record(").append(recordIntroductionLocation.get()).append(")");
        break;
      case EXPONENTIAL:
        sb.append("exp(").append(exponentialHeapLocation.get()).append(")");
        break;
      case CELL:
        sb.append("cell");
        break;
      case TYPE:
        sb.append("type(").append(type.get()).append(")");
        break;
      default:
        throw new IllegalStateException("Unknown slot type: " + slotType);
    }
    sb.append("@").append(producer);
    return sb.toString();
  }
}
