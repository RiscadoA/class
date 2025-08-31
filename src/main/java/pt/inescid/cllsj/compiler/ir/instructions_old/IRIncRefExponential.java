package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRIncRefExponential extends IRInstruction {
  private int exponential;
  private IRType type;

  public IRIncRefExponential(int exponential, IRType type) {
    this.exponential = exponential;
    this.type = type;
  }

  public int getExponential() {
    return exponential;
  }

  public IRType getType() {
    return type;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "incRefExponential(" + exponential + "[" + type + "])";
  }

  @Override
  public IRInstruction clone() {
    return new IRIncRefExponential(exponential, type);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    type = types.apply(type);
  }
}
