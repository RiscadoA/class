package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;

public class IRWriteExpression extends IRWrite {
  private IRExpression expr;

  public IRWriteExpression(IRDataLocation location, IRExpression expr) {
    super(location);
    this.expr = expr;
  }

  public IRExpression getExpression() {
    return expr;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteExpression(location, expr.clone());
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    expr.replaceDataLocations(replacer);
  }

  @Override
  public String toString() {
    return "writeExpression(" + location + ", " + expr + ")";
  }
}
