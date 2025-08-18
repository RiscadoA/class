package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPanic extends IRInstruction {
  private String msg;

  public IRPanic(String msg) {
    this.msg = msg;
  }

  public String getMsg() {
    return msg;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "panic(\"" + msg + "\")";
  }

  @Override
  public boolean usesRecord(int record) {
    return false;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
