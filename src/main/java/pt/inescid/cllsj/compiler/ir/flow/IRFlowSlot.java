package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;
import pt.inescid.cllsj.compiler.IRAnalyzer;

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

  // Instruction which pushed this slot.
  private Optional<IRFlowLocation> pusher;

  private IRFlowSlot(IRFlowLocation pusher) {
    this.pusher = Optional.of(pusher);
  }

  private IRFlowSlot() {
    this.pusher = Optional.empty();
  }

  public static IRFlowSlot unknown(IRFlowLocation pusher) {
    return new IRFlowSlot(pusher);
  }

  public static IRFlowSlot close(IRFlowLocation pusher) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.CLOSE;
    return slot;
  }

  public static IRFlowSlot tag(IRFlowLocation pusher, int tag) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.TAG;
    slot.tag = Optional.of(tag);
    return slot;
  }

  public static IRFlowSlot record(IRFlowLocation pusher, IRFlowLocation introductionLocation) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.RECORD;
    slot.recordIntroductionLocation = Optional.of(introductionLocation);
    return slot;
  }

  public static IRFlowSlot exponential(IRFlowLocation pusher, int heapLocation) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.EXPONENTIAL;
    slot.exponentialHeapLocation = Optional.of(heapLocation);
    return slot;
  }

  public static IRFlowSlot cell(IRFlowLocation pusher) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.CELL;
    return slot;
  }

  public static IRFlowSlot integer(IRFlowLocation pusher) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.INTEGER;
    return slot;
  }

  public static IRFlowSlot bool(IRFlowLocation pusher) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.BOOL;
    return slot;
  }

  public static IRFlowSlot string(IRFlowLocation pusher) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.STRING;
    return slot;
  }

  public static IRFlowSlot type(IRFlowLocation pusher, IRFlowType type) {
    IRFlowSlot slot = new IRFlowSlot(pusher);
    slot.slotType = Type.TYPE;
    slot.type = Optional.of(type);
    return slot;
  }

  public boolean isKnownRecord() {
    return recordIntroductionLocation.isPresent();
  }

  public Optional<IRFlowLocation> getPusher() {
    return pusher;
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

  public void markLost(IRAnalyzer analyzer, IRFlowState state) {
    if (recordIntroductionLocation.isPresent()) {
      state.getHeapRecord(recordIntroductionLocation.get()).markTotallyUnknown(analyzer, state);
    }
  }

  public IRFlowSlot merge(IRFlowSlot other) {
    IRFlowSlot merged;
    if (pusher.isEmpty() || !pusher.equals(other.pusher)) {
      merged = new IRFlowSlot();
    } else {
      merged = new IRFlowSlot(pusher.get());
    }

    if (this.slotType != other.slotType) {
      return merged;
    }
    merged.slotType = this.slotType;

    switch (merged.slotType) {
      case TAG:
        merged.tag = this.tag.equals(other.tag) ? this.tag : Optional.empty();
        if (merged.tag.isEmpty()) {
          merged.slotType = Type.UNKNOWN;
        }
        break;
      case RECORD:
        this.recordIntroductionLocation =
            this.recordIntroductionLocation.equals(other.recordIntroductionLocation)
                ? this.recordIntroductionLocation
                : Optional.empty();
        if (merged.recordIntroductionLocation.isEmpty()) {
          merged.slotType = Type.UNKNOWN;
        }
      case EXPONENTIAL:
        merged.exponentialHeapLocation =
            this.exponentialHeapLocation.equals(other.exponentialHeapLocation)
                ? this.exponentialHeapLocation
                : Optional.empty();
        if (merged.exponentialHeapLocation.isEmpty()) {
          merged.slotType = Type.UNKNOWN;
        }
        break;
      case TYPE:
        merged.type = this.type.equals(other.type) ? this.type : Optional.empty();
        if (merged.type.isEmpty()) {
          merged.slotType = Type.UNKNOWN;
        }
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
        break;
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
    sb.append("@").append(pusher);
    return sb.toString();
  }
}
