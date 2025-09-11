package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRCellS;

public class IRDecrementCell extends IRAccess {
  private IRCellS slot;

  public IRDecrementCell(IRDataLocation location, IRCellS slot) {
    super(location);
    this.slot = slot;
  }

  public IRCellS getSlot() {
    return slot;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRDecrementCell(location, slot);
  }

  @Override
  public String toString() {
    return "decrementCell(" + location + ", " + slot + ")";
  }
}
