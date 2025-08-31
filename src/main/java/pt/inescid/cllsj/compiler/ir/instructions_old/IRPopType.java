package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

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

  private int argRecord;
  private int argType;
  private Optional<Case> positive;
  private Optional<Case> negative;

  public IRPopType(
      int record, IRType recordType, int argRecord, int argType, Case positive, Case negative) {
    super(record, recordType);
    this.argRecord = argRecord;
    this.argType = argType;
    this.positive = Optional.ofNullable(positive);
    this.negative = Optional.ofNullable(negative);
  }

  public int getArgRecord() {
    return argRecord;
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
    StringBuilder sb = new StringBuilder();
    sb.append(argRecord).append(", ");
    sb.append(argType);
    if (positive.isPresent()) {
      sb.append(", +").append(positive.get().label);
    }
    if (negative.isPresent()) {
      sb.append(", -").append(negative.get().label);
    }
    return toString("popType", sb.toString());
  }

  @Override
  public IRInstruction clone() {
    Case pos =
        positive.isPresent() ? new Case(positive.get().label, positive.get().endPoints) : null;
    Case neg =
        negative.isPresent() ? new Case(negative.get().label, negative.get().endPoints) : null;
    return new IRPopType(getRecord(), getRecordType(), argRecord, argType, pos, neg);
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

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    super.renameRecords(renamer);
    argRecord = renamer.apply(argRecord);
  }
}
