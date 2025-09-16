package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public abstract class IRAccess extends IRInstruction {
  protected IRDataLocation location;

  public IRAccess(IRDataLocation location) {
    this.location = location;
  }

  public IRDataLocation getLocation() {
    return location;
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    location = replacer.apply(location);
  }
}
