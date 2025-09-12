package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public class IRBranchIsValue extends IRBranch {
  IRValueRequisites requisites;
  Case then;
  Case otherwise;

  public IRBranchIsValue(IRValueRequisites requisites, Case then, Case otherwise) {
    this.requisites = requisites;
    this.then = then;
    this.otherwise = otherwise;
  }

  public IRValueRequisites getRequisites() {
    return requisites;
  }

  public Case getThen() {
    return then;
  }

  public Case getOtherwise() {
    return otherwise;
  }

  @Override
  public List<Case> getCases() {
    return List.of(then, otherwise);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRBranchIsValue(requisites, then.clone(), otherwise.clone());
  }

  @Override
  public String toString() {
    return "branchIsValue(" + requisites + ", " + then + ", " + otherwise + ")";
  }
}
