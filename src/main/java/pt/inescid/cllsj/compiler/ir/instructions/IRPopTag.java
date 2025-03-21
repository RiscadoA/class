package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Map;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopTag extends IRInstruction {
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
  }

  private int record;
  private Map<Integer, Case> cases; // Cases for each tag.

  public IRPopTag(int record, Map<Integer, Case> cases) {
    this.record = record;
    this.cases = cases;
  }

  public int getRecord() {
    return record;
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
    String str = "popTag(" + record;
    for (Map.Entry<Integer, Case> entry : this.cases.entrySet()) {
      str += ", " + entry.getKey() + " -> " + entry.getValue().label + " (" + entry.getValue().endPoints + ")";
    }
    return str + ")";
  }
}
