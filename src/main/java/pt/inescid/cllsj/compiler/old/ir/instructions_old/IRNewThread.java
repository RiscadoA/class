package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRNewThread extends IRInstruction {
  private String label;

  public IRNewThread(String label) {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newThread(" + label + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRNewThread(label);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {}

  @Override
  public void renameLabels(Function<String, String> renamer) {
    label = renamer.apply(label);
  }
}
