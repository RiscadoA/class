package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;

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
  public void accept(IRInstructionVisitor visitor) {
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
}
