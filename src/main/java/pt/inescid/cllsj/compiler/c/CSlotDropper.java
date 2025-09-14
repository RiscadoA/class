package pt.inescid.cllsj.compiler.c;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRCellS;
import pt.inescid.cllsj.compiler.ir.slot.IRExponentialS;
import pt.inescid.cllsj.compiler.ir.slot.IRIntS;
import pt.inescid.cllsj.compiler.ir.slot.IRSessionS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotVisitor;
import pt.inescid.cllsj.compiler.ir.slot.IRStringS;
import pt.inescid.cllsj.compiler.ir.slot.IRTagS;
import pt.inescid.cllsj.compiler.ir.slot.IRTypeS;
import pt.inescid.cllsj.compiler.ir.slot.IRVarS;

public class CSlotDropper extends IRSlotVisitor {
  private CGenerator gen;
  private Function<IRTypeId, String> typeNode;
  private CAddress address;

  public static void drop(
      CGenerator gen, Function<IRTypeId, String> typeNode, CAddress address, IRSlot slot) {
    CSlotDropper dropper = new CSlotDropper();
    dropper.gen = gen;
    dropper.typeNode = typeNode;
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
    gen.putTypeNodeTraversal(
        slot.getTypeId(),
        typeNode,
        address,
        address -> drop(gen, typeNode, address, new IRExponentialS()),
        address -> drop(gen, typeNode, address, new IRStringS()));
  }
}
