package pt.inescid.cllsj.compiler.c;

import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRExponentialS;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRSessionS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotVisitor;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;
import pt.inescid.cllsj.compiler.ir.slot.IRTagS;
import pt.inescid.cllsj.compiler.ir.slot.IRTypeS;
import pt.inescid.cllsj.compiler.ir.slot.IRVarS;

public class CSlotCloner extends IRSlotVisitor {
  private CGenerator gen;
  private CAddress address;

  public static void clone(CGenerator gen, CAddress address, IRSlot slot) {
    CSlotCloner cloner = new CSlotCloner();
    cloner.gen = gen;
    cloner.address = address;
    slot.accept(cloner);
  }

  @Override
  public void visit(IRIntS slot) {}

  @Override
  public void visit(IRBoolS slot) {}

  @Override
  public void visit(IRStringS slot) {
    gen.putAssign(address.deref("char*"), "string_clone(" + address.deref("char*") + ")");
  }

  @Override
  public void visit(IRTagS slot) {}

  @Override
  public void visit(IRSessionS slot) {
    throw new IllegalArgumentException("Sessions cannot be cloned");
  }

  @Override
  public void visit(IRExponentialS slot) {
    gen.putIncrementExponential(address.deref("struct exponential*"));
  }

  @Override
  public void visit(IRTypeS slot) {
    throw new IllegalArgumentException("Types cannot be cloned");
  }

  @Override
  public void visit(IRVarS slot) {
    throw new IllegalArgumentException("Polymorphic slots cannot be cloned");
  }
}
