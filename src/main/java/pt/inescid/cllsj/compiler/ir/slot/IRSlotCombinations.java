package pt.inescid.cllsj.compiler.ir.slot;

import java.util.List;

public class IRSlotCombinations {
  private List<IRSlotSequence> sequences;

  public IRSlotCombinations(List<IRSlotSequence> sequences) {
    this.sequences = sequences;
  }

  public List<IRSlotSequence> list() {
    return sequences;
  }

  public IRSlotSequence get(int index) {
    return sequences.get(index);
  }

  public int size() {
    return sequences.size();
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (!sequences.isEmpty()) {
      sb.append(sequences.get(0).toString());
    }
    for (int i = 1; i < sequences.size(); i++) {
      sb.append(" + ").append(sequences.get(i).toString());
    }
    return sb.toString();
  }
}
