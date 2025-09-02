package pt.inescid.cllsj.compiler.ir.old.instructions_old;

import java.util.List;
import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.old.type.IRType;
import pt.inescid.cllsj.compiler.old.ir.IRInstructionVisitorOld;

public class IRCallProcess extends IRInstruction {
  public static class LinearArgument {
    private int sourceRecord;
    private int targetRecord;
    private IRType recordType;

    public LinearArgument(int sourceRecord, int targetRecord, IRType recordType) {
      this.sourceRecord = sourceRecord;
      this.targetRecord = targetRecord;
      this.recordType = recordType;
    }

    public int getSourceRecord() {
      return sourceRecord;
    }

    public int getTargetRecord() {
      return targetRecord;
    }

    public IRType getRecordType() {
      return recordType;
    }
  }

  public static class ExponentialArgument {
    private int sourceExponential;
    private int targetExponential;
    private IRType exponentialType;

    public ExponentialArgument(
        int sourceExponential, int targetExponential, IRType exponentialType) {
      this.sourceExponential = sourceExponential;
      this.targetExponential = targetExponential;
      this.exponentialType = exponentialType;
    }

    public int getSourceExponential() {
      return sourceExponential;
    }

    public int getTargetExponential() {
      return targetExponential;
    }

    public IRType getExponentialType() {
      return exponentialType;
    }
  }

  public static class TypeArgument {
    private IRType sourceType;
    private boolean sourceTypePolarity;
    private int targetType;

    public TypeArgument(IRType sourceType, boolean sourceTypePolarity, int targetType) {
      this.sourceType = sourceType;
      this.sourceTypePolarity = sourceTypePolarity;
      this.targetType = targetType;
    }

    public IRType getSourceType() {
      return sourceType;
    }

    public boolean getSourceTypePolarity() {
      return sourceTypePolarity;
    }

    public int getTargetType() {
      return targetType;
    }
  }

  private String processName;
  private List<LinearArgument> linearArguments;
  private List<ExponentialArgument> exponentialArguments;
  private List<TypeArgument> typeArguments;
  private boolean isEndPoint = true;

  public IRCallProcess(
      String processName,
      List<LinearArgument> linearArguments,
      List<ExponentialArgument> exponentialArguments,
      List<TypeArgument> typeArguments) {
    this.processName = processName;
    this.linearArguments = linearArguments;
    this.exponentialArguments = exponentialArguments;
    this.typeArguments = typeArguments;
  }

  public String getProcessName() {
    return processName;
  }

  public List<LinearArgument> getLinearArguments() {
    return linearArguments;
  }

  public List<ExponentialArgument> getExponentialArguments() {
    return exponentialArguments;
  }

  public List<TypeArgument> getTypeArguments() {
    return typeArguments;
  }

  public boolean isEndPoint() {
    return isEndPoint;
  }

  public void removeEndPoint() {
    isEndPoint = false;
  }

  @Override
  public void accept(IRInstructionVisitorOld visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "callProcess(" + processName;
    if (isEndPoint()) {
      str += ", end point";
    }
    for (TypeArgument arg : this.typeArguments) {
      str += ", T" + arg.getTargetType() + " <- " + arg.getSourceType();
    }
    for (LinearArgument arg : this.linearArguments) {
      str += ", L" + arg.getTargetRecord() + " <- " + arg.getSourceRecord();
    }
    for (ExponentialArgument arg : this.exponentialArguments) {
      str += ", E" + arg.getTargetExponential() + " <- " + arg.getSourceExponential();
    }
    return str + ")";
  }

  @Override
  public IRInstruction clone() {
    IRCallProcess clone =
        new IRCallProcess(
            processName,
            linearArguments.stream()
                .map(arg -> new LinearArgument(arg.sourceRecord, arg.targetRecord, arg.recordType))
                .toList(),
            exponentialArguments.stream()
                .map(
                    arg ->
                        new ExponentialArgument(
                            arg.sourceExponential, arg.targetExponential, arg.exponentialType))
                .toList(),
            typeArguments.stream()
                .map(
                    arg -> new TypeArgument(arg.sourceType, arg.sourceTypePolarity, arg.targetType))
                .toList());
    clone.isEndPoint = isEndPoint;
    return clone;
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    for (LinearArgument arg : linearArguments) {
      arg.sourceRecord = renamer.apply(arg.sourceRecord);
    }
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    for (ExponentialArgument arg : exponentialArguments) {
      arg.sourceExponential = renamer.apply(arg.sourceExponential);
    }
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    for (LinearArgument arg : linearArguments) {
      arg.recordType = types.apply(arg.recordType);
    }
    for (ExponentialArgument arg : exponentialArguments) {
      arg.exponentialType = types.apply(arg.exponentialType);
    }
    for (TypeArgument arg : typeArguments) {
      arg.sourceType = types.apply(arg.sourceType);
    }
  }
}
