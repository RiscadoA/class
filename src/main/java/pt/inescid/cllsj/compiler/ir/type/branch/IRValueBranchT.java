package pt.inescid.cllsj.compiler.ir.type.branch;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.type.IRBranchT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRValueBranchT extends IRBranchT {
  private IRValueRequisites requisites;
  private IRType value;
  private IRType notValue;

  public IRValueBranchT(IRValueRequisites requisites, IRType value, IRType notValue) {
    this.requisites = requisites;
    this.value = value;
    this.notValue = notValue;
  }

  public IRValueRequisites getRequisites() {
    return requisites;
  }

  public IRType getValue() {
    return value;
  }

  public IRType getNotValue() {
    return notValue;
  }

  @Override
  public List<IRType> getBranches() {
    return List.of(value, notValue);
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("value ").append(requisites);
    sb.append(" { yes: ").append(value);
    sb.append(", no: ").append(notValue).append(" }");
    return sb.toString();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRValueBranchT(requisites, value.withContinuation(cont), notValue.withContinuation(cont));
  }
}
