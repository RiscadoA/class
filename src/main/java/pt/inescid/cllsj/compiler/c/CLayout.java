package pt.inescid.cllsj.compiler.c;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.*;

public class CLayout {
  public CSize size;
  public CAlignment alignment;

  public CLayout(CSize size, CAlignment alignment) {
    this.size = size;
    this.alignment = alignment;
  }

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

  public static CLayout compute(
      IRSlotSequence sequence, CArchitecture arch, Function<IRTypeId, CLayout> typeLayoutProvider) {
    Visitor visitor = new Visitor(arch, typeLayoutProvider);
    for (IRSlot slot : sequence.list()) {
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
      layout.alignment = layout.alignment.max(elementAlignment);
      layout.size = layout.size.align(elementAlignment).add(elementSize);
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
    public void visit(IRTypeS slot) {
      visit(CSize.sizeOf("struct type_slot"), arch.pointerAlignment);
    }

    @Override
    public void visit(IRSessionS slot) {
      visit(arch.sessionSize(), arch.sessionAlignment());
    }

    @Override
    public void visit(IRVarS slot) {
      visit(typeLayoutProvider.apply(slot.getTypeId()));
    }
  }
}
