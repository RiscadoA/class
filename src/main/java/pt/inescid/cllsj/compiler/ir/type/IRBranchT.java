package pt.inescid.cllsj.compiler.ir.type;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public abstract class IRBranchT extends IRType {
  public abstract List<IRType> getBranches();

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }
}
