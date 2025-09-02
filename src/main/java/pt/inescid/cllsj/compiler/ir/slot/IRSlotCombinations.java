package pt.inescid.cllsj.compiler.ir.slot;

import java.util.List;

public class IRSlotCombinations {
  private List<IRSlotSequence> sequences;

  public static final IRSlotCombinations EMPTY = new IRSlotCombinations(List.of());

  public IRSlotCombinations(List<IRSlotSequence> sequences) {
    this.sequences = sequences;
  }

  public static IRSlotCombinations of(IRSlot slot) {
    return new IRSlotCombinations(List.of(IRSlotSequence.of(slot)));
  }

  public static IRSlotCombinations of(IRSlotSequence sequence) {
    return new IRSlotCombinations(List.of(sequence));
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

  public IRSlotCombinations prefix(IRSlot slot) {
    return new IRSlotCombinations(sequences.stream().map(seq -> seq.prefix(slot)).toList());
  }

  public IRSlotCombinations prefix(IRSlotSequence sequence) {
    return new IRSlotCombinations(sequences.stream().map(seq -> sequence.prefix(seq)).toList());
  }

  public IRSlotCombinations prefix(IRSlotCombinations combinations) {
    return new IRSlotCombinations(
        combinations.sequences.stream()
            .flatMap(seq1 -> sequences.stream().map(seq2 -> seq1.prefix(seq2)))
            .toList());
  }

  public IRSlotCombinations merge(IRSlotCombinations other) {
    List<IRSlotSequence> newSequences = sequences.stream().toList();
    newSequences.addAll(other.sequences);
    return new IRSlotCombinations(newSequences);
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
