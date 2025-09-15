package pt.inescid.cllsj.compiler.ir.slot;

import java.util.ArrayList;
import java.util.List;

public class IRSlotCombinations {
  private List<IRSlotTree> trees;

  public static final IRSlotCombinations EMPTY = new IRSlotCombinations(List.of());

  public IRSlotCombinations(List<IRSlotTree> trees) {
    this.trees = trees;
  }

  public static IRSlotCombinations of(IRSlot slot) {
    return new IRSlotCombinations(List.of(IRSlotTree.of(slot)));
  }

  public static IRSlotCombinations of(IRSlot... slots) {
    return new IRSlotCombinations(List.of(IRSlotTree.of(slots)));
  }

  public static IRSlotCombinations of(IRSlotTree... trees) {
    return new IRSlotCombinations(List.of(trees));
  }

  public List<IRSlotTree> list() {
    return trees;
  }

  public IRSlotTree get(int index) {
    return trees.get(index);
  }

  public int size() {
    return trees.size();
  }

  // public IRSlotCombinations prefix(IRSlot slot) {
  //   if (trees.isEmpty()) {
  //     return of(slot);
  //   }
  //   return new IRSlotCombinations(trees.stream().map(tree ->
  // IRSlotTree.of(slot).suffix(tree)).toList());
  // }

  // public IRSlotCombinations prefix(IRSlotCombinations combinations) {
  //   if (trees.isEmpty()) {
  //     return combinations;
  //   }
  //   return new IRSlotCombinations(
  //       combinations.trees.stream()
  //           .flatMap(seq1 -> trees.stream().map(seq2 -> seq1.prefix(seq2)))
  //           .toList());
  // }

  public IRSlotCombinations suffix(IRSlot slot) {
    if (trees.isEmpty()) {
      return of(slot);
    }
    return new IRSlotCombinations(
        trees.stream().map(tree -> tree.suffix(IRSlotTree.of(slot))).toList());
  }

  public IRSlotCombinations suffix(IRSlotSequence sequence) {
    if (trees.isEmpty()) {
      return of(IRSlotTree.of(sequence));
    }
    return new IRSlotCombinations(
        trees.stream().map(tree -> tree.suffix(IRSlotTree.of(sequence))).toList());
  }

  public IRSlotCombinations suffix(IRSlotCombinations combinations) {
    if (combinations.trees.isEmpty()) {
      return this;
    }
    if (trees.isEmpty()) {
      return combinations;
    }
    return new IRSlotCombinations(
        combinations.trees.stream()
            .flatMap(seq1 -> trees.stream().map(seq2 -> seq2.suffix(seq1)))
            .toList());
  }

  public IRSlotCombinations merge(IRSlotCombinations other) {
    List<IRSlotTree> newTrees = new ArrayList<>(trees);
    newTrees.addAll(other.trees);
    return new IRSlotCombinations(newTrees);
  }

  public boolean isEmpty() {
    return trees.isEmpty() || (trees.size() == 1 && trees.get(0).isLeaf());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("<");
    if (!trees.isEmpty()) {
      sb.append(trees.get(0).toString());
    }
    for (int i = 1; i < trees.size(); i++) {
      sb.append(" + ").append(trees.get(i).toString());
    }
    sb.append(">");
    return sb.toString();
  }
}
