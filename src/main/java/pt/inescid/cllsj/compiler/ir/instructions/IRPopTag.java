package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.Map;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopTag extends IRInstruction {
  private int record;
  private Map<Integer, String> labels; // Labels to jump to for each tag.

  public IRPopTag(int record, Map<Integer, String> labels) {
    this.record = record;
    this.labels = labels;
  }

  public int getRecord() {
    return record;
  }

  public Map<Integer, String> getLabels() {
    return labels;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "popTag(" + record;
    for (Map.Entry<Integer, String> entry : this.labels.entrySet()) {
      str += ", " + entry.getKey() + " -> " + entry.getValue();
    }
    return str + ")";
  }
}
