package pt.inescid.cllsj.compiler.ir.type;

import java.util.Optional;
import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRVarT extends IRType {
  // Index of the type variable, counting from the end.
  // I.e., index 0 refers to the last introduced type, such that in `sendty B; sendty A; send B; A`,
  // the type variable A has index 0, and the type variable B as index 1.
  private int type;

  // If the type needs a preceding 'IRFlipT', but we were not able to determine it at type
  // conversion time,
  // (e.g., because the type variable polarity is only known later in the program flow), this
  // variable
  // holds the polarity which, if held by the variable, would require a preceding 'IRFlipT'.
  private Optional<Boolean> flipPolarity;

  public IRVarT(int type, Optional<Boolean> flipPolarity) {
    this.type = type;
    this.flipPolarity = flipPolarity;
  }

  public int getType() {
    return type;
  }

  // Checks if a 'IRFlipT' should have been preceding this variable, given the actual polarity of
  // the variable.
  public boolean hasPrecedingFlip(boolean varPolarity) {
    return flipPolarity.isPresent() && flipPolarity.get() == varPolarity;
  }

  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String fs = flipPolarity.map(p -> p ? "+" : "-").orElse("");
    return "var" + fs + " " + type;
  }

  @Override
  public IRType removeLastSlot() {
      throw new UnsupportedOperationException("Type is only known at runtime");
  }
}
