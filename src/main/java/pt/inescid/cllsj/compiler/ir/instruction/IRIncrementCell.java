package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRIncrementCell extends IRAccess {
  public IRIncrementCell(IRDataLocation location) {
    super(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRIncrementCell(location);
  }

  @Override
  public String toString() {
    return "incrementCell(" + location + ")";
  }
}
