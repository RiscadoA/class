package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRDecrementCell extends IRAccess {
  public IRDecrementCell(IRDataLocation location) {
    super(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRDecrementCell(location);
  }

  @Override
  public String toString() {
    return "decrementCell(" + location + ")";
  }
}
