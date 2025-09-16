package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRBranchTag extends IRBranch {
  private IRDataLocation location;
  private List<Case> cases;

  public IRBranchTag(IRDataLocation location, List<Case> cases) {
    this.location = location;
    this.cases = cases;
  }

  public IRDataLocation getLocation() {
    return location;
  }

  @Override
  public List<Case> getCases() {
    return cases;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRBranchTag(location, cases.stream().map(Case::clone).toList());
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    location = replacer.apply(location);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("branchTag(").append(location);
    for (Case c : cases) {
      sb.append(", ").append(c);
    }
    sb.append(")");
    return sb.toString();
  }
}
