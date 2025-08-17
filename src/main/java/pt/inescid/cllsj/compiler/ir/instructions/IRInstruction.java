package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public abstract class IRInstruction {
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  public abstract boolean usesRecord(int record);

  public abstract void renameRecords(Function<Integer, Integer> renamer);
}
