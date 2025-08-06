package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;

public class IRBranchOnValue extends IRInstruction {
  private IRValueRequisites requisites;
  private String isValue;
  private String isNotValue;

  public IRBranchOnValue(IRValueRequisites requisites, String isValue, String isNotValue) {
    this.requisites = requisites;
    this.isValue = isValue;
    this.isNotValue = isNotValue;
  }

  public IRValueRequisites getRequisites() {
    return requisites;
  }

  public String getIsValue() {
    return isValue;
  }

  public String getIsNotValue() {
    return isNotValue;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "branchOnValue(" + requisites + ", " + isValue + ", " + isNotValue + ")";
  }
}
