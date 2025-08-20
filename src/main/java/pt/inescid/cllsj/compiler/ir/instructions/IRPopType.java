package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopType extends IRPop {
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

    public void modifyEndPoints(int n) {
      this.endPoints += n;
    }
  }

  private int argType;
  private Optional<Case> positive;
  private Optional<Case> negative;

  public IRPopType(int record, int argType, Case positive, Case negative) {
    super(record);
    this.argType = argType;
    this.positive = Optional.ofNullable(positive);
    this.negative = Optional.ofNullable(negative);
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

  public int getEndPoints() {
    return Math.max(
        positive.map(Case::getEndPoints).orElse(0), negative.map(Case::getEndPoints).orElse(0));
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("popType(");
    sb.append(getRecord()).append(", ");
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

  @Override
  public IRInstruction clone() {
    Case pos =
        positive.isPresent() ? new Case(positive.get().label, positive.get().endPoints) : null;
    Case neg =
        negative.isPresent() ? new Case(negative.get().label, negative.get().endPoints) : null;
    return new IRPopType(getRecord(), argType, pos, neg);
  }

  @Override
  public void renameLabels(Function<String, String> renamer) {
    if (positive.isPresent()) {
      positive =
          Optional.of(new Case(renamer.apply(positive.get().label), positive.get().endPoints));
    }
    if (negative.isPresent()) {
      negative =
          Optional.of(new Case(renamer.apply(negative.get().label), negative.get().endPoints));
    }
  }
}
