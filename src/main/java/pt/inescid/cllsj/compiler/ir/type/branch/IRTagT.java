package pt.inescid.cllsj.compiler.ir.type.branch;

import java.util.ArrayList;
import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRBranchT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRTagT extends IRBranchT {
  private List<IRType> choices;

  public IRTagT(List<IRType> choices) {
    this.choices = choices;
  }

  @Override
  public List<IRType> getBranches() {
    return choices;
  }

  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String result = "tag";
    for (int i = 0; i < choices.size(); i++) {
      result += " { " + i + ": " + choices.get(i) + " }";
    }
    return result;
  }

  @Override
  public IRType withContinuation(IRType cont) {
    List<IRType> newChoices = new ArrayList<>();
    for (IRType choice : choices) {
      newChoices.add(choice.withContinuation(cont));
    }
    return new IRTagT(newChoices);
  }
}
