package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;

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
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    expression.replaceDataLocations(replacer);
  }

  @Override
  public IRInstruction clone() {
    return new IRPrint(expression.clone(), newLine);
  }
}
