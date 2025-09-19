package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRLaunchThread extends IRInstruction {
  private IRCodeLocation location;
  private List<IRDataLocation> passedLockedCells;

  public IRLaunchThread(IRCodeLocation location, List<IRDataLocation> passedLockedCells) {
    this.location = location;
    this.passedLockedCells = passedLockedCells;
  }

  public IRCodeLocation getLocation() {
    return location;
  }

  public List<IRDataLocation> getPassedLockedCells() {
    return passedLockedCells;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("launchThread(").append(location);
    for (IRDataLocation cell : passedLockedCells) {
      sb.append(", pass ").append(cell);
    }
    sb.append(")");
    return sb.toString();
  }

  @Override
  public IRInstruction clone() {
    return new IRLaunchThread(location, new ArrayList<>(passedLockedCells));
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    super.replaceCodeLocations(replacer);
    location = replacer.apply(location);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    passedLockedCells.replaceAll(replacer::apply);
  }
}
