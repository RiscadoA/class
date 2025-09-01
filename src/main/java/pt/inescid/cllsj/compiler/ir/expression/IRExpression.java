package pt.inescid.cllsj.compiler.ir.expression;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public abstract class IRExpression {
  public abstract IRSlot getSlot();

  public abstract void accept(IRExpressionVisitor visitor);

  public abstract IRExpression clone();

  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {}
}
