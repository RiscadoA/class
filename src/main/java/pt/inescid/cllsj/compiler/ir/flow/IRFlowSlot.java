package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.type.*;

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
  private Optional<IRFlowRecord> record = Optional.empty();
  private Optional<IRFlowExponential> exponential = Optional.empty();
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

  public static IRFlowSlot expression(IRExpression expression) {
    IRFlowSlot slot = new IRFlowSlot();
    if (expression.getType() instanceof IRIntT) {
      slot.slotType = Type.INTEGER;
    } else if (expression.getType() instanceof IRBoolT) {
      slot.slotType = Type.BOOL;
    } else if (expression.getType() instanceof IRStringT) {
      slot.slotType = Type.STRING;
    } else {
      slot.slotType = Type.UNKNOWN;
    }
    return slot;
  }

  public static IRFlowSlot record(IRFlowRecord record) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.RECORD;
    slot.record = Optional.of(record);
    return slot;
  }

  public static IRFlowSlot exponential(IRFlowExponential exponential) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.slotType = Type.EXPONENTIAL;
    slot.exponential = Optional.of(exponential);
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
    return record.isPresent();
  }

  public IRFlowRecord getRecord() {
    return record.orElseThrow(() -> new IllegalStateException("Slot does not hold a known record"));
  }

  public boolean isKnownTag() {
    return tag.isPresent();
  }

  public int getTag() {
    return tag.orElseThrow(() -> new IllegalStateException("Slot does not hold a known tag"));
  }

  public boolean isKnownExponential() {
    return exponential.isPresent();
  }

  public IRFlowExponential getExponential() {
    return exponential.orElseThrow(() -> new IllegalStateException("Slot does not hold a known exponential"));
  }

  public boolean isValue() {
    return slotType == Type.INTEGER || slotType == Type.BOOL || slotType == Type.STRING || slotType == Type.EXPONENTIAL;
  }

  public boolean isKnownType() {
    return type.isPresent();
  }

  public IRFlowType getType() {
    return type.orElseThrow(() -> new IllegalStateException("Slot does not hold a known type"));
  }

  public void markLost(IRFlowState state) {
    if (record.isPresent()) {
      record.get().markTotallyUnknown(state);
    }
  }

  public IRFlowSlot merge(IRFlowState.Cloner cloner, IRFlowSlot other) {
    if (this.slotType != other.slotType) {
      return unknown();
    }

    switch (this.slotType) {
      case TAG:
        return this.getTag() == other.getTag() ? this : unknown();
      case RECORD:
        return record(this.getRecord().merge(cloner, other.getRecord()));
      case EXPONENTIAL:
        return exponential(this.getExponential().merge(cloner, other.getExponential()));
      case TYPE:
        return type(this.getType().merge(other.getType()));
      default:
        return this;
    }
  }

  public IRFlowSlot clone(IRFlowState.Cloner cloner) {
    IRFlowSlot clone = new IRFlowSlot();
    clone.slotType = this.slotType;
    
    switch (this.slotType) {
      case RECORD:
        clone.record = Optional.of(this.getRecord().clone(cloner));
        break;
      case EXPONENTIAL:
        clone.exponential = Optional.of(this.getExponential().clone());
        break;
      case TAG:
        clone.tag = this.tag;
        break;
      case TYPE:
        clone.type = Optional.of(this.getType().clone());
        break;
      default:
        break;
    }
    
    return clone;
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
        return "record(" + record.get().getIndex() + ")";
      case EXPONENTIAL:
        return "exponential(" + exponential.get() + ")";
      case CELL:
        return "cell";
      case TYPE:
        return "type(" + type.get() + ")";
      default:
        throw new IllegalStateException("Unknown slot type: " + slotType);
    }
  }
}
