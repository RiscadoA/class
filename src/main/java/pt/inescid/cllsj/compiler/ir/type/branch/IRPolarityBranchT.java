package pt.inescid.cllsj.compiler.ir.type.branch;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRBranchT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPolarityBranchT extends IRBranchT {
  // Index of the type variable, starting from the last introduced type variable.
  private int type;
  private IRType positive;
  private IRType negative;

  public IRPolarityBranchT(int type, IRType positive, IRType negative) {
    this.type = type;
    this.positive = positive;
    this.negative = negative;
  }

  public int getType() {
    return type;
  }

  public IRType getPositive() {
    return positive;
  }

  public IRType getNegative() {
    return negative;
  }

  @Override
  public List<IRType> getBranches() {
    return List.of(positive, negative);
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("polarity ").append(type);
    sb.append(" { +: ").append(positive);
    sb.append(", -: ").append(negative).append(" }");
    return sb.toString();
  }

  @Override
  public IRType withContinuation(IRType cont) {
    return new IRPolarityBranchT(type, positive.withContinuation(cont), negative.withContinuation(cont));
  }
}
