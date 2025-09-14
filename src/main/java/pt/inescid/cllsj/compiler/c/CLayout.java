package pt.inescid.cllsj.compiler.c;

import java.util.function.Function;
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

  // Computes the maximum size the given slot combinations can take
  public static CLayout compute(
      IRSlotCombinations combinations,
      CArchitecture arch,
      Function<IRTypeId, CLayout> typeLayoutProvider) {
    CLayout layout = new CLayout(CSize.zero(), CAlignment.one());
    for (IRSlotSequence sequence : combinations.list()) {
      CLayout sequenceLayout = compute(sequence, arch, typeLayoutProvider);
      layout.size = layout.size.max(sequenceLayout.size);
      layout.alignment = layout.alignment.max(sequenceLayout.alignment);
    }
    return layout;
  }

  // Computes the current size of the given slot tree
  // public static CLayout compute(
  //     IRSlotTree tree,
  //     CArchitecture arch,
  //     Function<IRTypeId, CLayout> typeLayoutProvider,
  //     Function<IRValueRequisites, CCondition> typeIsValue,
  //     BiFunction<IRSlotStaticOffset, Integer, CCondition> isTag) {
  //   return compute(tree, arch, typeLayoutProvider, typeIsValue, isTag, IRSlotStaticOffset.ZERO);
  // }

  // private static CLayout compute(
  //     IRSlotTree tree,
  //     CArchitecture arch,
  //     Function<IRTypeId, CLayout> typeLayoutProvider,
  //     Function<IRValueRequisites, CCondition> typeIsValue,
  //     BiFunction<IRSlotStaticOffset, Integer, CCondition> isTag,
  //     IRSlotStaticOffset offset) {
  //   if (tree.isLeaf()) {
  //     return CLayout.ZERO;
  //   } else if (tree.isUnary()) {
  //     // A unary node contains a single slot and a child tree
  //     IRSlotTree.Unary unary = (IRSlotTree.Unary) tree;
  //     IRSlot slot = unary.singleHead().get();
  //     CLayout childLayout =
  //         compute(
  //             unary.child(),
  //             arch,
  //             typeLayoutProvider,
  //             typeIsValue,
  //             isTag,
  //             offset.advance(slot, unary.child().combinations()));
  //     CLayout slotLayout = compute(slot, arch, typeLayoutProvider);
  //     return new CLayout(
  //         slotLayout.size.align(childLayout.alignment).add(childLayout.size),
  //         slotLayout.alignment.max(childLayout.alignment));
  //   } else if (tree.isTag()) {
  //     // Branch depending on the tag value
  //     IRSlotTree.Tag tag = (IRSlotTree.Tag) tree;
  //     CLayout result = CLayout.ZERO;
  //     for (int i = 0; i < tag.cases().size(); ++i) {
  //       IRSlotTree caseTree = tag.cases().get(i);
  //       CCondition condition = isTag.apply(offset, i);
  //       IRSlotStaticOffset caseOffset = offset.advance(new IRTagS(), caseTree.combinations());
  //       CLayout caseLayout =
  //           compute(caseTree, arch, typeLayoutProvider, typeIsValue, isTag, caseOffset);
  //       result = condition.ternary(caseLayout, result);
  //     }
  //     return result;
  //   } else if (tree.isIsValue()) {
  //     // Branch depending on if the value requisites are met or not
  //     IRSlotTree.IsValue isValue = (IRSlotTree.IsValue) tree;
  //     CCondition condition = typeIsValue.apply(isValue.requisites());
  //     IRSlotStaticOffset contOffset =
  //         IRSlotStaticOffset.of(offset.getPast().suffix(isValue.value().combinations().merge(isValue.notValue().combinations())),
  //             isValue.cont().combinations());
  //     CLayout contLayout =
  //         compute(isValue.cont(), arch, typeLayoutProvider, typeIsValue, isTag, contOffset);
  //     return 
  //   } else {
  //     throw new IllegalArgumentException("Unsupported slot tree: " + tree);
  //   }
  // }

  public static CLayout compute(
      IRSlotSequence sequence, CArchitecture arch, Function<IRTypeId, CLayout> typeLayoutProvider) {
    Visitor visitor = new Visitor(arch, typeLayoutProvider);
    for (IRSlot slot : sequence.list().reversed()) {
      slot.accept(visitor);
    }
    return visitor.layout;
  }

  public static CLayout compute(
      IRSlot slot, CArchitecture arch, Function<IRTypeId, CLayout> typeLayoutProvider) {
    Visitor visitor = new Visitor(arch, typeLayoutProvider);
    slot.accept(visitor);
    return visitor.layout;
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
      // Adds the element before the existing layout
      layout.size = elementSize.align(layout.alignment).add(layout.size);
      layout.alignment = layout.alignment.max(elementAlignment);
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
