package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRNewSession extends IRInstruction {
  private int record; // Index of the record to be initialized.
  private Optional<String> label; // Label for the initial continuation.

  public IRNewSession(int record, String label) {
    this.record = record;
    this.label = Optional.ofNullable(label);
  }

  public int getRecord() {
    return record;
  }

  public Optional<String> getLabel() {
    return label;
  }

  public void setLabel(Optional<String> label) {
    this.label = label;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("newSession(");
    sb.append(record);
    if (label.isPresent()) {
      sb.append(", ").append(label.get());
    }
    sb.append(")");
    return sb.toString();
  }

  @Override
  public IRInstruction clone() {
    IRNewSession clone = new IRNewSession(record, null);
    clone.setLabel(label);
    return clone;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    record = renamer.apply(record);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}

  @Override
  public void renameLabels(Function<String, String> renamer) {
    if (label.isPresent()) {
      label = Optional.of(renamer.apply(label.get()));
    } else {
      label = Optional.empty();
    }
  }
}
