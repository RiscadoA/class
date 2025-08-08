package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopType extends IRInstruction {
  private int record;
  private int argType;
  private Optional<String> positiveLabel;
  private Optional<String> negativeLabel;

  public IRPopType(int record, int argType, String positiveLabel, String negativeLabel) {
    this.record = record;
    this.argType = argType;
    this.positiveLabel = Optional.of(positiveLabel);
    this.negativeLabel = Optional.of(negativeLabel);
  }

  public int getRecord() {
    return record;
  }

  public int getArgType() {
    return argType;
  }

  public Optional<String> getPositiveLabel() {
    return positiveLabel;
  }

  public Optional<String> getNegativeLabel() {
    return negativeLabel;
  }

  public void removePositiveLabel() {
    this.positiveLabel = Optional.empty();
  }

  public void removeNegativeLabel() {
    this.negativeLabel = Optional.empty();
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("popType(");
    sb.append(record).append(", ");
    sb.append(argType);
    if (positiveLabel.isPresent()) {
      sb.append(", +").append(positiveLabel.get());
    }
    if (negativeLabel.isPresent()) {
      sb.append(", -").append(negativeLabel.get());
    }
    sb.append(")");
    return sb.toString();
  }
}
