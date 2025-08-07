package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;

import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.type.*;

public class IRFlowSlot {
  private static enum Type {
    UNKNOWN, CLOSE, TAG, INTEGER, BOOLEAN, STRING, RECORD, EXPONENTIAL
  }

  private Type type;
  private Optional<IRFlowRecord> record = Optional.empty();
  private Optional<Integer> tag = Optional.empty();

  public static IRFlowSlot unknown() {
    return new IRFlowSlot();
  }

  public static IRFlowSlot close() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.type = Type.CLOSE;
    return slot;
  }

  public static IRFlowSlot tag(int tag) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.type = Type.TAG;
    slot.tag = Optional.of(tag);
    return slot;
  }

  public static IRFlowSlot expression(IRExpression expression) {
    IRFlowSlot slot = new IRFlowSlot();
    if (expression.getType() instanceof IRIntT) {
      slot.type = Type.INTEGER;
    } else if (expression.getType() instanceof IRBoolT) {
      slot.type = Type.BOOLEAN;
    } else if (expression.getType() instanceof IRStringT) {
      slot.type = Type.STRING;
    } else {
      slot.type = Type.UNKNOWN;
    }
    return slot;
  }

  public static IRFlowSlot record(IRFlowRecord record) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.type = Type.RECORD;
    slot.record = Optional.of(record);
    return slot;
  }

  public static IRFlowSlot exponential() {
    IRFlowSlot slot = new IRFlowSlot();
    slot.type = Type.EXPONENTIAL;
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

  @Override
  public String toString() {
    switch (type) {
      case UNKNOWN: return "unknown";
      case CLOSE: return "close";
      case TAG: return "tag(" + tag.get() + ")";
      case INTEGER: return "integer";
      case BOOLEAN: return "boolean";
      case STRING: return "string";
      case RECORD: return "record(" + record.get().getIndex() + ")";
      case EXPONENTIAL: return "exponential";
      default: return "???";
    }
  }
}
