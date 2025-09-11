package pt.inescid.cllsj.compiler.ir.slot;

import java.util.ArrayList;
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

  public static IRSlotCombinations of(IRSlot... slots) {
    return new IRSlotCombinations(List.of(IRSlotSequence.of(slots)));
  }

  public static IRSlotCombinations of(IRSlotSequence... sequences) {
    return new IRSlotCombinations(List.of(sequences));
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
    if (sequences.isEmpty()) {
      return of(slot);
    }
    return new IRSlotCombinations(sequences.stream().map(seq -> seq.prefix(slot)).toList());
  }

  public IRSlotCombinations prefix(IRSlotSequence sequence) {
    if (sequences.isEmpty()) {
      return of(sequence);
    }
    return new IRSlotCombinations(sequences.stream().map(seq -> sequence.prefix(seq)).toList());
  }

  public IRSlotCombinations prefix(IRSlotCombinations combinations) {
    if (sequences.isEmpty()) {
      return combinations;
    }
    return new IRSlotCombinations(
        combinations.sequences.stream()
            .flatMap(seq1 -> sequences.stream().map(seq2 -> seq1.prefix(seq2)))
            .toList());
  }

  public IRSlotCombinations suffix(IRSlot slot) {
    if (sequences.isEmpty()) {
      return of(slot);
    }
    return new IRSlotCombinations(sequences.stream().map(seq -> seq.suffix(slot)).toList());
  }

  public IRSlotCombinations suffix(IRSlotSequence sequence) {
    if (sequences.isEmpty()) {
      return of(sequence);
    }
    return new IRSlotCombinations(sequences.stream().map(seq -> seq.suffix(sequence)).toList());
  }

  public IRSlotCombinations suffix(IRSlotCombinations combinations) {
    if (sequences.isEmpty()) {
      return combinations;
    }
    return new IRSlotCombinations(
        combinations.sequences.stream()
            .flatMap(seq1 -> sequences.stream().map(seq2 -> seq2.suffix(seq1)))
            .toList());
  }

  public IRSlotCombinations merge(IRSlotCombinations other) {
    List<IRSlotSequence> newSequences = new ArrayList<>(sequences);
    newSequences.addAll(other.sequences);
    return new IRSlotCombinations(newSequences);
  }

  public boolean isEmpty() {
    return sequences.isEmpty() || (sequences.size() == 1 && sequences.get(0).size() == 0);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("<");
    if (!sequences.isEmpty()) {
      sb.append(sequences.get(0).toString());
    }
    for (int i = 1; i < sequences.size(); i++) {
      sb.append(" + ").append(sequences.get(i).toString());
    }
    sb.append(">");
    return sb.toString();
  }
}
