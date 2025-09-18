package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRAcquireCell extends IRAccess {
  public IRAcquireCell(IRDataLocation location) {
    super(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRAcquireCell(location);
  }

  @Override
  public String toString() {
    return "acquireCell(" + location + ")";
  }
}
