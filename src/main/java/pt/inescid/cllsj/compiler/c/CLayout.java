package pt.inescid.cllsj.compiler.c;

import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.*;

public class CLayout {
  public static final CLayout ZERO = new CLayout(CSize.zero(), CAlignment.one());

  public CSize size;
  public CAlignment alignment;

  public CLayout(CSize size, CAlignment alignment) {
    this.size = size;
    this.alignment = alignment;
  }

  // Computes the current size of the given slot tree (i.e., taking into account chosen tags)
  public static CLayout computeCurrent(
      IRSlotTree tree,
      CArchitecture arch,
      Function<IRTypeId, CLayout> typeLayoutProvider,
      Function<IRTypeFlagRequisites, CCondition> requisitesMet,
      BiFunction<CSize, Integer, CCondition> isTag) {
    return computeCurrent(tree, arch, typeLayoutProvider, requisitesMet, isTag, CSize.zero());
  }

  private static CLayout computeCurrent(
      IRSlotTree tree,
      CArchitecture arch,
      Function<IRTypeId, CLayout> typeLayoutProvider,
      Function<IRTypeFlagRequisites, CCondition> requisitesMet,
      BiFunction<CSize, Integer, CCondition> isTag,
      CSize offset) {
    if (tree.isLeaf()) {
      return CLayout.ZERO;
    } else if (tree.isUnary()) {
      // A unary node contains a single slot and a child tree
      IRSlotTree.Unary unary = (IRSlotTree.Unary) tree;
      IRSlot slot = unary.singleHead().get();
      CLayout slotLayout = compute(slot, arch, typeLayoutProvider);
      CAlignment childAlignment =
          computeMaximum(unary.child(), arch, typeLayoutProvider, requisitesMet).alignment;
      CLayout childLayout =
          computeCurrent(
              unary.child(),
              arch,
              typeLayoutProvider,
              requisitesMet,
              isTag,
              offset.add(slotLayout.size).align(childAlignment));
      return slotLayout.concat(childLayout);
    } else if (tree.isTag()) {
      // Branch depending on the tag value
      IRSlotTree.Tag tag = (IRSlotTree.Tag) tree;
      CLayout result = CLayout.ZERO;
      CLayout slotLayout = compute(new IRTagS(), arch, typeLayoutProvider);
      for (int i = 0; i < tag.cases().size(); ++i) {
        IRSlotTree caseTree = tag.cases().get(i);
        CCondition condition = isTag.apply(offset, i);
        CAlignment caseAlignment =
            computeMaximum(caseTree, arch, typeLayoutProvider, requisitesMet).alignment;
        CSize caseOffset = offset.add(slotLayout.size).align(caseAlignment);
        CLayout caseLayout =
            computeCurrent(caseTree, arch, typeLayoutProvider, requisitesMet, isTag, caseOffset);
        result = condition.ternary(caseLayout, result);
      }
      return slotLayout.concat(result);
    } else if (tree.isType()) {
      // Branch depending on if the flag requisites are met or not
      IRSlotTree.Type type = (IRSlotTree.Type) tree;
      CCondition condition = requisitesMet.apply(type.requisites());
      CLayout valueLayout =
          computeCurrent(type.met(), arch, typeLayoutProvider, requisitesMet, isTag, offset);
      CLayout notValueLayout =
          computeCurrent(type.unmet(), arch, typeLayoutProvider, requisitesMet, isTag, offset);
      return condition.ternary(valueLayout, notValueLayout);
    } else {
      throw new IllegalArgumentException("Unsupported slot tree: " + tree);
    }
  }

  // Computes the maximum size the given slot tree can take (i.e., maximizing over all tags)
  public static CLayout computeMaximum(
      IRSlotTree tree,
      CArchitecture arch,
      Function<IRTypeId, CLayout> typeLayoutProvider,
      Function<IRTypeFlagRequisites, CCondition> requisitesMet) {
    if (tree.isLeaf()) {
      return CLayout.ZERO;
    } else if (tree.isUnary()) {
      // A unary node contains a single slot and a child tree
      IRSlotTree.Unary unary = (IRSlotTree.Unary) tree;
      IRSlot slot = unary.singleHead().get();
      CLayout childLayout = computeMaximum(unary.child(), arch, typeLayoutProvider, requisitesMet);
      CLayout slotLayout = compute(slot, arch, typeLayoutProvider);
      return slotLayout.concat(childLayout);
    } else if (tree.isTag()) {
      // Branch depending on the tag value
      IRSlotTree.Tag tag = (IRSlotTree.Tag) tree;
      CLayout result = CLayout.ZERO;
      for (int i = 0; i < tag.cases().size(); ++i) {
        IRSlotTree caseTree = tag.cases().get(i);
        CLayout caseLayout = computeMaximum(caseTree, arch, typeLayoutProvider, requisitesMet);
        result =
            new CLayout(
                result.size.max(caseLayout.size), result.alignment.max(caseLayout.alignment));
      }
      return compute(new IRTagS(), arch, typeLayoutProvider).concat(result);
    } else if (tree.isType()) {
      // Branch depending on if the requisites are met or not
      IRSlotTree.Type type = (IRSlotTree.Type) tree;
      CCondition condition = requisitesMet.apply(type.requisites());
      CLayout valueLayout = computeMaximum(type.met(), arch, typeLayoutProvider, requisitesMet);
      CLayout notValueLayout =
          computeMaximum(type.unmet(), arch, typeLayoutProvider, requisitesMet);
      return condition.ternary(valueLayout, notValueLayout);
    } else {
      throw new IllegalArgumentException("Unsupported slot tree: " + tree);
    }
  }

  // Computes the maximum size the given slot combinations can take
  public static CLayout computeMaximum(
      IRSlotCombinations combinations,
      CArchitecture arch,
      Function<IRTypeId, CLayout> typeLayoutProvider,
      Function<IRTypeFlagRequisites, CCondition> requisitesMet) {
    CLayout layout = new CLayout(CSize.zero(), CAlignment.one());
    for (IRSlotTree tree : combinations.list()) {
      CLayout treeLayout = computeMaximum(tree, arch, typeLayoutProvider, requisitesMet);
      layout.size = layout.size.max(treeLayout.size);
      layout.alignment = layout.alignment.max(treeLayout.alignment);
    }
    return layout;
  }

  public static CLayout compute(
      IRSlot slot, CArchitecture arch, Function<IRTypeId, CLayout> typeLayoutProvider) {
    Visitor visitor = new Visitor(arch, typeLayoutProvider);
    slot.accept(visitor);
    return visitor.layout;
  }

  private CLayout concat(CLayout after) {
    return new CLayout(
        this.size.align(after.alignment).add(after.size), this.alignment.max(after.alignment));
  }

  private static class Visitor extends IRSlotVisitor {
    private CLayout layout = new CLayout(CSize.zero(), CAlignment.one());
    private CArchitecture arch;
    private Function<IRTypeId, CLayout> typeLayoutProvider;

    public Visitor(CArchitecture arch, Function<IRTypeId, CLayout> typeLayoutProvider) {
      this.arch = arch;
      this.typeLayoutProvider = typeLayoutProvider;
    }

    private void visit(CSize elementSize, CAlignment elementAlignment) {
      layout = new CLayout(elementSize, elementAlignment);
    }

    private void visit(CLayout elementLayout) {
      visit(elementLayout.size, elementLayout.alignment);
    }

    @Override
    public void visit(IRIntS slot) {
      visit(arch.intSize, arch.intAlignment);
    }

    @Override
    public void visit(IRBoolS slot) {
      visit(arch.unsignedCharSize, arch.unsignedCharAlignment);
    }

    @Override
    public void visit(IRStringS slot) {
      visit(arch.pointerSize, arch.pointerAlignment);
    }

    @Override
    public void visit(IRTagS slot) {
      visit(arch.unsignedCharSize, arch.unsignedCharAlignment);
    }

    @Override
    public void visit(IRExponentialS slot) {
      visit(arch.pointerSize, arch.pointerAlignment);
    }

    @Override
    public void visit(IRCellS slot) {
      visit(arch.pointerSize, arch.pointerAlignment);
    }

    @Override
    public void visit(IRTypeS slot) {
      visit(arch.typeSize(), arch.typeAlignment());
    }

    @Override
    public void visit(IRSessionS slot) {
      visit(arch.pointerSize, arch.pointerAlignment);
    }

    @Override
    public void visit(IRVarS slot) {
      visit(typeLayoutProvider.apply(slot.getTypeId()));
    }
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    CLayout other = (CLayout) obj;
    return size.equals(other.size) && alignment.equals(other.alignment);
  }

  @Override
  public int hashCode() {
    int result = size.hashCode();
    result = 31 * result + alignment.hashCode();
    return result;
  }
}
