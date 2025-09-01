package pt.inescid.cllsj.compiler.c;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.slot.*;

public class CLayout {
  public CSize size;
  public CAlignment alignment;

  public CLayout(CSize size, CAlignment alignment) {
    this.size = size;
    this.alignment = alignment;
  }

  public static CLayout compute(
      IRSlotSequence sequence, CArchitecture arch, Function<Integer, CLayout> varToLayout) {
    Visitor visitor = new Visitor(arch, varToLayout);
    for (IRSlot slot : sequence.list()) {
      slot.accept(visitor);
    }
    return visitor.layout;
  }

  private static class Visitor extends IRSlotVisitor {
    private CLayout layout = new CLayout(CSize.zero(), CAlignment.one());
    private CArchitecture arch;
    private Function<Integer, CLayout> varToLayout;

    public Visitor(CArchitecture arch, Function<Integer, CLayout> varToLayout) {
      this.arch = arch;
      this.varToLayout = varToLayout;
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
      CLayout passedLayout = CLayout.compute(slot.getPassedSlots(), arch, varToLayout);

      visit(
          arch.pointerSize.align(passedLayout.alignment).add(passedLayout.size),
          arch.pointerAlignment.max(passedLayout.alignment));
    }

    @Override
    public void visit(IRVarS slot) {
      visit(varToLayout.apply(slot.getTypeId()));
    }
  }
}
