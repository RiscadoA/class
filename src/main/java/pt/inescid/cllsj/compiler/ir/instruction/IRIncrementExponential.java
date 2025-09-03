package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRIncrementExponential extends IRAccess {
  public IRIncrementExponential(IRDataLocation location) {
    super(location);
  }

  @Override
  public String toString() {
    return "incrementExponential(" + location + ")";
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRIncrementExponential(location);
  }
}
