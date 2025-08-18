package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;

public class IRNewExponentialExpression extends IRInstruction {
  private int exponential;
  private IRExpression expression;

  public IRNewExponentialExpression(int exponential, IRExpression expression) {
    this.exponential = exponential;
    this.expression = expression;
  }

  public int getExponential() {
    return exponential;
  }

  public IRExpression getExpression() {
    return expression;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "newExponentialExpression(" + exponential + ", " + expression + ")";
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }
}
