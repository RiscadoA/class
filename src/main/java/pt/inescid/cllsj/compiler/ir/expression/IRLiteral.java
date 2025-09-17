package pt.inescid.cllsj.compiler.ir.expression;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public abstract class IRLiteral extends IRExpression {
  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {}
}
