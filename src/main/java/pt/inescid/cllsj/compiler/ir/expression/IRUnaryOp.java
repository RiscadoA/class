package pt.inescid.cllsj.compiler.ir.expression;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRUnaryOp extends IRExpression {
  protected IRExpression inner;

  public IRUnaryOp(IRExpression inner) {
    this.inner = inner;
  }

  @Override
  public IRSlot getSlot() {
    return inner.getSlot();
  }

  public IRExpression getInner() {
    return inner;
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    inner.replaceDataLocations(replacer);
  }
}
