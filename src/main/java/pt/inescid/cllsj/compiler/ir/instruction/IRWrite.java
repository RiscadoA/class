package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRDataLocation;

public abstract class IRWrite extends IRAccess {
  public IRWrite(IRDataLocation location) {
    super(location);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    location = replacer.apply(location);
  }
}
