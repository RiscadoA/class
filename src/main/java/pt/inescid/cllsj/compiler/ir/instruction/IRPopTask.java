package pt.inescid.cllsj.compiler.ir.instruction;

public class IRPopTask extends IRInstruction {
  private boolean isEndPoint;

  public IRPopTask(boolean isEndPoint) {
    this.isEndPoint = isEndPoint;
  }

  public boolean isEndPoint() {
    return isEndPoint;
  }

  public void removeEndPoint() {
    isEndPoint = false;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRPopTask(isEndPoint);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder("popTask(");
    if (isEndPoint()) {
      sb.append("end point");
    }
    sb.append(")");
    return sb.toString();
  }
}
