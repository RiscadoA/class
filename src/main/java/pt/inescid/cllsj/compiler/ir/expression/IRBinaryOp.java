package pt.inescid.cllsj.compiler.ir.expression;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRBinaryOp extends IRExpression {
  protected IRExpression lhs;
  protected IRExpression rhs;

  public IRBinaryOp(IRExpression lhs, IRExpression rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }

  @Override
  public IRSlot getSlot() {
    return lhs.getSlot();
  }

  public IRExpression getLhs() {
    return lhs;
  }

  public IRExpression getRhs() {
    return rhs;
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    lhs.replaceDataLocations(replacer);
    rhs.replaceDataLocations(replacer);
  }
}
