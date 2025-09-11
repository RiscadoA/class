package pt.inescid.cllsj.compiler.c;

import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRCellS;
import pt.inescid.cllsj.compiler.ir.slot.IRExponentialS;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRKnownVarS;
import pt.inescid.cllsj.compiler.ir.slot.IRSessionS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotVisitor;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;
import pt.inescid.cllsj.compiler.ir.slot.IRTagS;
import pt.inescid.cllsj.compiler.ir.slot.IRTypeS;
import pt.inescid.cllsj.compiler.ir.slot.IRVarS;

public class CSlotDropper extends IRSlotVisitor {
  private CGenerator gen;
  private CAddress address;

  public static void drop(CGenerator gen, CAddress address, IRSlot slot) {
    CSlotDropper dropper = new CSlotDropper();
    dropper.gen = gen;
    dropper.address = address;
    slot.accept(dropper);
  }

  @Override
  public void visit(IRIntS slot) {}

  @Override
  public void visit(IRBoolS slot) {}

  @Override
  public void visit(IRStringS slot) {
    gen.putStatement("string_drop(" + address.deref("char*") + ")");
  }

  @Override
  public void visit(IRTagS slot) {}

  @Override
  public void visit(IRSessionS slot) {
    throw new IllegalArgumentException("Sessions cannot be dropped");
  }

  @Override
  public void visit(IRExponentialS slot) {
    gen.putDecrementExponential(address.deref("struct exponential*"));
  }

  @Override
  public void visit(IRCellS slot) {
    gen.putDecrementCell(address);
  }

  @Override
  public void visit(IRTypeS slot) {
    throw new IllegalArgumentException("Types cannot be dropped");
  }

  @Override
  public void visit(IRVarS slot) {
    throw new IllegalArgumentException("Polymorphic slots cannot be dropped");
  }

  @Override
  public void visit(IRKnownVarS slot) {
    throw new IllegalArgumentException("Polymorphic slots cannot be dropped");
  }
}
