package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopType extends IRInstruction {
  public static class Case {
    private String label;
    private int endPoints;

    public Case(String label, int endPoints) {
      this.label = label;
      this.endPoints = endPoints;
    }

    public String getLabel() {
      return label;
    }

    public int getEndPoints() {
      return endPoints;
    }

    public void subtractEndPoints(int n) {
      this.endPoints -= n;
    }
  }

  private int record;
  private int argType;
  private Optional<Case> positive;
  private Optional<Case> negative;

  public IRPopType(int record, int argType, Case positive, Case negative) {
    this.record = record;
    this.argType = argType;
    this.positive = Optional.of(positive);
    this.negative = Optional.of(negative);
  }

  public int getRecord() {
    return record;
  }

  public int getArgType() {
    return argType;
  }

  public Optional<Case> getPositive() {
    return positive;
  }

  public Optional<Case> getNegative() {
    return negative;
  }

  public void removePositive() {
    this.positive = Optional.empty();
  }

  public void removeNegative() {
    this.negative = Optional.empty();
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
    if (positive.isPresent()) {
      sb.append(", +").append(positive.get().label);
    }
    if (negative.isPresent()) {
      sb.append(", -").append(negative.get().label);
    }
    sb.append(")");
    return sb.toString();
  }
}
