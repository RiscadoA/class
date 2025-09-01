package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRIncreaseExponentialReferences extends IRAccess {
  public IRIncreaseExponentialReferences(IRDataLocation location) {
    super(location);
  }

  @Override
  public String toString() {
    return "increaseExponentialReferences(" + location + ")";
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRIncreaseExponentialReferences(location);
  }
}
