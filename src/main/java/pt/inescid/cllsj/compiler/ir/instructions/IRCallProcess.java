package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.List;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRCallProcess extends IRInstruction {
  public static class LinearArgument {
    private int sourceRecord;
    private int targetRecord;

    public LinearArgument(int sourceRecord, int targetRecord) {
      this.sourceRecord = sourceRecord;
      this.targetRecord = targetRecord;
    }

    public int getSourceRecord() {
      return sourceRecord;
    }

    public int getTargetRecord() {
      return targetRecord;
    }
  }

  public static class ExponentialArgument {
    private int sourceExponential;
    private int targetExponential;

    public ExponentialArgument(int sourceExponential, int targetExponential) {
      this.sourceExponential = sourceExponential;
      this.targetExponential = targetExponential;
    }

    public int getSourceExponential() {
      return sourceExponential;
    }

    public int getTargetExponential() {
      return targetExponential;
    }
  }

  public static class TypeArgument {
    private IRType sourceType;
    private IRValueRequisites sourceTypeValueRequisites;
    private boolean sourceTypePolarity;
    private int targetType;

    public TypeArgument(
        IRType sourceType,
        IRValueRequisites sourceTypeValueRequisites,
        boolean sourceTypePolarity,
        int targetType) {
      this.sourceType = sourceType;
      this.sourceTypeValueRequisites = sourceTypeValueRequisites;
      this.sourceTypePolarity = sourceTypePolarity;
      this.targetType = targetType;
    }

    public IRType getSourceType() {
      return sourceType;
    }

    public IRValueRequisites getSourceTypeValueRequisites() {
      return sourceTypeValueRequisites;
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
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "callProcess(" + processName;
    if (isEndPoint()) {
      str += ", end point";
    }
    for (TypeArgument arg : this.typeArguments) {
      str +=
          ", T"
              + arg.getTargetType()
              + " <- "
              + arg.getSourceType()
              + " "
              + arg.getSourceTypeValueRequisites();
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
                .map(arg -> new LinearArgument(arg.sourceRecord, arg.targetRecord))
                .toList(),
            exponentialArguments.stream()
                .map(arg -> new ExponentialArgument(arg.sourceExponential, arg.targetExponential))
                .toList(),
            typeArguments.stream()
                .map(
                    arg ->
                        new TypeArgument(
                            arg.sourceType,
                            arg.sourceTypeValueRequisites,
                            arg.sourceTypePolarity,
                            arg.targetType))
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
  public void substituteTypes(
      Function<IRType, IRType> types, Function<IRValueRequisites, IRValueRequisites> requisites) {
    for (TypeArgument arg : typeArguments) {
      arg.sourceType = types.apply(arg.sourceType);
      arg.sourceTypeValueRequisites = requisites.apply(arg.sourceTypeValueRequisites);
    }
  }
}
