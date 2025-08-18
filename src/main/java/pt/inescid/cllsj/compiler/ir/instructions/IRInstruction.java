package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public abstract class IRInstruction {
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  public abstract void renameRecords(Function<Integer, Integer> renamer);

  public abstract void renameExponentials(Function<Integer, Integer> renamer);

  public boolean usesRecord(int record) {
    AtomicBoolean result = new AtomicBoolean(false);
    renameRecords(
        r -> {
          if (r == record) {
            result.set(true);
          }
          return r;
        });
    return result.get();
  }

  public boolean usesExponential(int exponential) {
    AtomicBoolean result = new AtomicBoolean(false);
    renameExponentials(
        e -> {
          if (e == exponential) {
            result.set(true);
          }
          return e;
        });
    return result.get();
  }
}
