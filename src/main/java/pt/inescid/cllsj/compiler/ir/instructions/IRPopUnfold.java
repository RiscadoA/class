package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPopUnfold extends IRPop {
  public IRPopUnfold(int record) {
    super(record);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "popUnfold(" + getRecord() + ")";
  }
}
