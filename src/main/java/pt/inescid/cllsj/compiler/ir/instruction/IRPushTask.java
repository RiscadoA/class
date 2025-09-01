package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;

public class IRPushTask extends IRInstruction {
  private IRCodeLocation location;

  public IRPushTask(IRCodeLocation location) {
    this.location = location;
  }

  public IRCodeLocation getLocation() {
    return location;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushTask(" + location + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRPushTask(location);
  }
}
