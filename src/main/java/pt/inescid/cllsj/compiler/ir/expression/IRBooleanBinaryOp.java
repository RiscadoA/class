package pt.inescid.cllsj.compiler.ir.expression;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRBoolS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRBooleanBinaryOp extends IRBinaryOp {
  public IRBooleanBinaryOp(IRExpression lhs, IRExpression rhs) {
    super(lhs, rhs);
  }

  @Override
  public IRSlot getSlot() {
    return new IRBoolS();
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    lhs.replaceDataLocations(replacer);
    rhs.replaceDataLocations(replacer);
  }
}
