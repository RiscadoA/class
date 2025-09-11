package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRLockCell extends IRAccess {
  public IRLockCell(IRDataLocation location) {
    super(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRLockCell(location);
  }

  @Override
  public String toString() {
    return "lockCell(" + location + ")";
  }
}
