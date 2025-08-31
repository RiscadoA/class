package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPopTag extends IRPop {
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

  private Map<Integer, Case> cases; // Cases for each tag.

  public IRPopTag(int record, IRType recordType, Map<Integer, Case> cases) {
    super(record, recordType);
    this.cases = cases;
  }

  public Map<Integer, Case> getCases() {
    return cases;
  }

  public int getEndPoints() {
    int max = 0;
    for (Case c : cases.values()) {
      max = Math.max(max, c.getEndPoints());
    }
    return max;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (Map.Entry<Integer, Case> entry : this.cases.entrySet()) {
      if (!sb.isEmpty()) {
        sb.append(", ");
      }
      sb.append(entry.getKey()).append(" -> ");
      sb.append(entry.getValue().label + " (" + entry.getValue().endPoints + ")");
    }
    return toString("popTag", sb.toString());
  }

  @Override
  public IRInstruction clone() {
    return new IRPopTag(
        getRecord(),
        getRecordType(),
        cases.entrySet().stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey, e -> new Case(e.getValue().label, e.getValue().endPoints))));
  }

  @Override
  public void renameLabels(Function<String, String> renamer) {
    cases =
        cases.entrySet().stream()
            .collect(
                Collectors.toMap(
                    Map.Entry::getKey,
                    e -> new Case(renamer.apply(e.getValue().label), e.getValue().endPoints)));
  }
}
