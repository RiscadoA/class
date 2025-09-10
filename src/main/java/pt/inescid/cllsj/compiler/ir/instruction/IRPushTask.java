package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;

public class IRPushTask extends IRInstruction {
  private IRCodeLocation location;
  private boolean concurrent;

  public IRPushTask(IRCodeLocation location, boolean concurrent) {
    this.location = location;
    this.concurrent = concurrent;
  }

  public IRCodeLocation getLocation() {
    return location;
  }

  public boolean isConcurrent() {
    return concurrent;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushTask(" + location + (concurrent ? ", concurrent" : "" ) + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRPushTask(location, concurrent);
  }
}
