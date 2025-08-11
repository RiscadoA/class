package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Map;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

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

    public void subtractEndPoints(int n) {
      this.endPoints -= n;
    }
  }

  private Map<Integer, Case> cases; // Cases for each tag.

  public IRPopTag(int record, Map<Integer, Case> cases) {
    super(record);
    this.cases = cases;
  }

  public Map<Integer, Case> getCases() {
    return cases;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "popTag(" + getRecord();
    for (Map.Entry<Integer, Case> entry : this.cases.entrySet()) {
      str +=
          ", "
              + entry.getKey()
              + " -> "
              + entry.getValue().label
              + " ("
              + entry.getValue().endPoints
              + ")";
    }
    return str + ")";
  }
}
