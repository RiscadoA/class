package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;

public class IRJump extends IRInstruction {
  private IRCodeLocation location;

  public IRJump(IRCodeLocation location) {
    this.location = location;
  }

  public IRCodeLocation getLocation() {
    return location;
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    super.replaceCodeLocations(replacer);
    location = replacer.apply(location);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRJump(location);
  }

  @Override
  public String toString() {
    return "jump(" + location + ")";
  }
}
