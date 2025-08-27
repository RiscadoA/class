package pt.inescid.cllsj.compiler.ir.type;

import java.util.List;
import java.util.function.BiFunction;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRTagT extends IRType {
  private List<IRType> choices;

  public IRTagT(List<IRType> choices) {
    this.choices = choices;
  }

  public List<IRType> getChoices() {
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
  public IRType substituteVar(int index, int offset, BiFunction<Integer, IRVarT, IRType> types) {
    List<IRType> newChoices =
        choices.stream().map(choice -> choice.substituteVar(index, offset, types)).toList();
    return new IRTagT(newChoices);
  }

  @Override
  public boolean equals(IRType other) {
    return other instanceof IRTagT && ((IRTagT) other).getChoices().equals(getChoices());
  }
}
