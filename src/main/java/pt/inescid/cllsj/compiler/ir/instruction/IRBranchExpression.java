package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRBranchExpression extends IRBranch {
  IRExpression expression;
  Case then;
  Case otherwise;

  public IRBranchExpression(IRExpression expression, Case then, Case otherwise) {
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
  public List<Case> getCases() {
    return List.of(then, otherwise);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRBranchExpression(expression.clone(), then.clone(), otherwise.clone());
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    expression.replaceDataLocations(replacer);
  }

  @Override
  public String toString() {
    return "branchExpression(" + expression + ", " + then + ", " + otherwise + ")";
  }
}
