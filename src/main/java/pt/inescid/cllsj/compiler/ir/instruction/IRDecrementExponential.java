package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRDecrementExponential extends IRAccess {
  public IRDecrementExponential(IRDataLocation location) {
    super(location);
  }

  @Override
  public String toString() {
    return "decrementExponential(" + location + ")";
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRDecrementExponential(location);
  }
}
