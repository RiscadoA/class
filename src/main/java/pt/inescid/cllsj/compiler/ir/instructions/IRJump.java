package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRJump extends IRInstruction {
  private String label;

  public IRJump(String label) {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "jump(" + label + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return false;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}
}
