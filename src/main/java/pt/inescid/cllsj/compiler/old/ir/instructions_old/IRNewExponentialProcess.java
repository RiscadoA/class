package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.List;
import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.instructions_old.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.old.instructions_old.IRCallProcess.TypeArgument;
import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRNewExponentialProcess extends IRInstruction {
  private int exponential;
  private IRType type;
  private String processName;
  private List<IRCallProcess.ExponentialArgument> exponentialArguments;
  private List<IRCallProcess.TypeArgument> typeArguments;

  public IRNewExponentialProcess(
      int exponential,
      IRType type,
      String processName,
      List<IRCallProcess.ExponentialArgument> exponentialArguments,
      List<IRCallProcess.TypeArgument> typeArguments) {
    this.exponential = exponential;
    this.type = type;
    this.processName = processName;
    this.exponentialArguments = exponentialArguments;
    this.typeArguments = typeArguments;
  }

  public int getExponential() {
    return exponential;
  }

  public IRType getType() {
    return type;
  }

  public String getProcessName() {
    return processName;
  }

  public List<IRCallProcess.ExponentialArgument> getExponentialArguments() {
    return exponentialArguments;
  }

  public List<TypeArgument> getTypeArguments() {
    return typeArguments;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "newExponentialProcess(" + exponential + "[" + type + "], " + processName;
    for (TypeArgument arg : this.typeArguments) {
      str += ", T" + arg.getTargetType() + " <- " + arg.getSourceType();
    }
    for (ExponentialArgument arg : this.exponentialArguments) {
      str += ", E" + arg.getTargetExponential() + " <- " + arg.getSourceExponential();
    }
    return str + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRNewExponentialProcess(
        exponential,
        type,
        processName,
        exponentialArguments.stream()
            .map(
                arg ->
                    new ExponentialArgument(
                        arg.getSourceExponential(),
                        arg.getTargetExponential(),
                        arg.getExponentialType()))
            .toList(),
        typeArguments.stream()
            .map(
                arg ->
                    new TypeArgument(
                        arg.getSourceType(), arg.getSourceTypePolarity(), arg.getTargetType()))
            .toList());
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {}

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);

    for (int i = 0; i < exponentialArguments.size(); ++i) {
      ExponentialArgument arg = exponentialArguments.get(i);
      exponentialArguments.set(
          i,
          new ExponentialArgument(
              renamer.apply(arg.getSourceExponential()),
              arg.getTargetExponential(),
              arg.getExponentialType()));
    }
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    for (int i = 0; i < exponentialArguments.size(); ++i) {
      ExponentialArgument arg = exponentialArguments.get(i);
      exponentialArguments.set(
          i,
          new ExponentialArgument(
              arg.getSourceExponential(),
              arg.getTargetExponential(),
              types.apply(arg.getExponentialType())));
    }
    for (int i = 0; i < typeArguments.size(); ++i) {
      TypeArgument arg = typeArguments.get(i);
      typeArguments.set(
          i,
          new TypeArgument(
              types.apply(arg.getSourceType()), arg.getSourceTypePolarity(), arg.getTargetType()));
    }
  }
}
