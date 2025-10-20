package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRWriteScanChar extends IRWrite {
  public IRWriteScanChar(IRDataLocation location) {
    super(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteScanChar(location);
  }

  @Override
  public String toString() {
    return "writeScanChar(" + location + ")";
  }
}
