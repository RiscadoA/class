package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.expressions.IRExpression;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

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

    public void modifyEndPoints(int n) {
      this.endPoints += n;
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
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "branch(" + expression + ", " + then + ", " + otherwise + ")";
  }

  public int getEndPoints() {
    return Math.max(then.getEndPoints(), otherwise.getEndPoints());
  }

  @Override
  public IRInstruction clone() {
    return new IRBranch(
        expression.clone(),
        new Case(then.getLabel(), then.getEndPoints()),
        new Case(otherwise.getLabel(), otherwise.getEndPoints()));
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
  public void renameLabels(Function<String, String> renamer) {
    then.label = renamer.apply(then.label);
    otherwise.label = renamer.apply(otherwise.label);
  }
}
