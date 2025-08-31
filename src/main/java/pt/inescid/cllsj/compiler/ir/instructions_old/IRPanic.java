package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitorOld;

public class IRPanic extends IRInstruction {
  private String msg;

  public IRPanic(String msg) {
    this.msg = msg;
  }

  public String getMsg() {
    return msg;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "panic(\"" + msg + "\")";
  }

  @Override
  public IRInstruction clone() {
    return new IRPanic(msg);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}
}
