package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public abstract class IRInstruction {
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  public abstract IRInstruction clone();

  public abstract void renameRecords(Function<Integer, Integer> renamer);

  public abstract void renameExponentials(Function<Integer, Integer> renamer);

  public void renameLabels(Function<String, String> renamer) {}

  public void substituteTypes(Function<IRType, IRType> types) {}

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

  public boolean usesLabel(String label) {
    AtomicBoolean result = new AtomicBoolean(false);
    renameLabels(
        l -> {
          if (l.equals(label)) {
            result.set(true);
          }
          return l;
        });
    return result.get();
  }
}
