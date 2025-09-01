package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRDecreaseExponentialReferences extends IRAccess {
  public IRDecreaseExponentialReferences(IRDataLocation location) {
    super(location);
  }

  @Override
  public String toString() {
    return "decreaseExponentialReferences(" + location + ")";
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRDecreaseExponentialReferences(location);
  }
}
