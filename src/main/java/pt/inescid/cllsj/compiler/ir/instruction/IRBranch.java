package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;

public abstract class IRBranch extends IRInstruction {
  public static class Case {
    private IRCodeLocation location;
    private int endPoints;

    public Case(IRCodeLocation location, int endPoints) {
      this.location = location;
      this.endPoints = endPoints;
    }

    public IRCodeLocation getLocation() {
      return location;
    }

    public int getEndPoints() {
      return endPoints;
    }

    public void setEndPoints(int endPoints) {
      this.endPoints = endPoints;
    }

    public Case clone() {
      return new Case(location, endPoints);
    }

    @Override
    public String toString() {
      return location + ":" + endPoints;
    }
  }

  public abstract List<Case> getCases();

  public int getMaxEndPoints() {
    return getCases().stream().mapToInt(Case::getEndPoints).max().orElse(0);
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    for (Case c : getCases()) {
      c.location = replacer.apply(c.location);
    }
  }
}
