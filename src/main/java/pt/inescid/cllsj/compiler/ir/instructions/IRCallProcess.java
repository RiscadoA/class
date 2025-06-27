package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
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
    private int targetType;

    public TypeArgument(IRType sourceType, int targetType) {
      this.sourceType = sourceType;
      this.targetType = targetType;
    }

    public IRType getSourceType() {
      return sourceType;
    }

    public int getTargetType() {
      return targetType;
    }
  }

  private String processName;
  private List<LinearArgument> linearArguments;
  private List<ExponentialArgument> exponentialArguments;
  private List<TypeArgument> typeArguments;

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

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "callProcess(" + processName;
    for (TypeArgument arg : this.typeArguments) {
      str += ", T" + arg.getTargetType() + " <- " + arg.getTargetType();
    }
    for (LinearArgument arg : this.linearArguments) {
      str += ", L" + arg.getTargetRecord() + " <- " + arg.getSourceRecord();
    }
    for (ExponentialArgument arg : this.exponentialArguments) {
      str += ", E" + arg.getTargetExponential() + " <- " + arg.getSourceExponential();
    }
    return str + ")";
  }
}
