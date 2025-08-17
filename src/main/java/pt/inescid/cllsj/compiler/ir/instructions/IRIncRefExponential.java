package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRIncRefExponential extends IRInstruction {
  private int exponential;

  public IRIncRefExponential(int exponential) {
    this.exponential = exponential;
  }

  public int getExponential() {
    return exponential;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "incRefExponential(" + exponential + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return false;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}
}
