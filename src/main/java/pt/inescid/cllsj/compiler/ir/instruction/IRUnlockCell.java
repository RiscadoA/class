package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRUnlockCell extends IRAccess {
  public IRUnlockCell(IRDataLocation location) {
    super(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRUnlockCell(location);
  }

  @Override
  public String toString() {
    return "unlockCell(" + location + ")";
  }
}
