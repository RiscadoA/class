package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.IRDataLocation;

public abstract class IRAccess extends IRInstruction {
  protected IRDataLocation location;

  public IRAccess(IRDataLocation location) {
    this.location = location;
  }

  public IRDataLocation getLocation() {
    return location;
  }
}
