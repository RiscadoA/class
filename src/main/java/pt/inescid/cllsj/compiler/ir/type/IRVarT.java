package pt.inescid.cllsj.compiler.ir.type;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRVarT extends IRType {
  // Index of the type variable, counting from the end.
  // I.e., index 0 refers to the last introduced type, such that in `sendty B; sendty A; send B; A`,
  // the type variable A has index 0, and the type variable B as index 1.
  private int type;

  public IRVarT(int type) {
    this.type = type;
  }

  public int getType() {
    return type;
  }

  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "var " + type;
  }
}
