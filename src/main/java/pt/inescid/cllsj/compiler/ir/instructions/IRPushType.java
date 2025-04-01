package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushType extends IRInstruction {
  private int record;
  private boolean isPositive;

  public IRPushType(int record, boolean isPositive) {
    this.record = record;
    this.isPositive = isPositive;
  }

  public int getRecord() {
    return record;
  }

  public boolean isPositive() {
    return isPositive;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "pushType(" + record + ", " + (isPositive ? "positive" : "negative") + ")";
  }
}
