package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;

public class IRBranch extends IRInstruction {
  public static class Case {
    private String label;
    private int endPoints;

    public Case(String label, int endPoints) {
      this.label = label;
      this.endPoints = endPoints;
    }

    public String getLabel() {
      return label;
    }

    public int getEndPoints() {
      return endPoints;
    }

    @Override
    public String toString() {
      return label + " (" + endPoints + ")";
    }
  }

  private IRExpression expression;
  private Case then;
  private Case otherwise;

  public IRBranch(IRExpression expression, Case then, Case otherwise) {
    this.expression = expression;
    this.then = then;
    this.otherwise = otherwise;
  }

  public IRExpression getExpression() {
    return expression;
  }

  public Case getThen() {
    return then;
  }

  public Case getOtherwise() {
    return otherwise;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "branch(" + expression + ", " + then + ", " + otherwise + ")";
  }
}
