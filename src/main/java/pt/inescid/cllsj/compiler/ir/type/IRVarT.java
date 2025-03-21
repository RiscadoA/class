package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRVarT extends IRType {
  private int type;

  public IRVarT(int type) {
    this.type = type;
  }

  public int getType() {
    return type;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "var " + type;
  }
}
