package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.expressions.IRExpression;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRPrint extends IRInstruction {
  private IRExpression expression;
  private boolean newLine;

  public IRPrint(IRExpression expression, boolean newLine) {
    this.expression = expression;
    this.newLine = newLine;
  }

  public IRExpression getExpression() {
    return expression;
  }

  public boolean hasNewLine() {
    return newLine;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String result = "print";
    if (newLine) {
      result += "Line";
    }
    return result + "(" + expression + ")";
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    expression.renameRecords(renamer);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    expression.renameExponentials(renamer);
  }

  @Override
  public IRInstruction clone() {
    return new IRPrint(expression.clone(), newLine);
  }
}
