package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDropId;

public class IRDeferDrop extends IRInstruction {
  private IRDropId dropId;

  public IRDeferDrop(IRDropId dropId) {
    this.dropId = dropId;
  }

  public IRDropId getDropId() {
    return dropId;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRDeferDrop(dropId);
  }

  @Override
  public String toString() {
    return "deferDrop(" + dropId + ")";
  }

  @Override
  public void replaceDropIds(Function<IRDropId, IRDropId> replacer) {
    super.replaceDropIds(replacer);
    dropId = replacer.apply(dropId);
  }
}
