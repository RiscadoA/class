package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;

public class IRFlowSlot {
  private Optional<IRFlowRecord> record = Optional.empty();

  public static IRFlowSlot unknown() {
    return new IRFlowSlot();
  }

  public static IRFlowSlot record(IRFlowRecord record) {
    IRFlowSlot slot = new IRFlowSlot();
    slot.record = Optional.of(record);
    return slot;
  }

  public boolean isKnownRecord() {
    return record.isPresent();
  }

  public IRFlowRecord getRecord() {
    return record.orElseThrow(
        () -> new IllegalStateException("Slot does not hold a known record"));
  }
}
