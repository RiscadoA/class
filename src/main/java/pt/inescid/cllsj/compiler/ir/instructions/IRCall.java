package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.List;
import java.util.Optional;

import pt.inescid.cllsj.compiler.ir.IRVisitor;

public class IRCall extends IRInstruction {
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

  public static class TypeArgument {
    private Optional<Integer> sourceType;
    private int targetType;

    // If sourceRecord is not present, the target type will have this polarity.
    // If it is present, it's polarity will be xor'ed with this value.
    private boolean sourcePolarity;
    
    public TypeArgument(int sourceType, int targetType, boolean isDual) {
      this.sourceType = Optional.of(sourceType);
      this.targetType = targetType;
      this.sourcePolarity = isDual;
    }

    public TypeArgument(boolean sourcePolarity, int targetType) {
      this.sourceType = Optional.empty();
      this.targetType = targetType;
      this.sourcePolarity = sourcePolarity;
    }

    public Optional<Integer> getSourceType() {
      return sourceType;
    }

    public int getTargetType() {
      return targetType;
    }

    public boolean getSourcePolarity() {
      assert sourceType.isEmpty();
      return sourcePolarity;
    }

    public boolean isDual() {
      assert sourceType.isPresent();
      return sourcePolarity;
    }
  }

  private String processName;
  private List<LinearArgument> linearArguments;
  private List<TypeArgument> typeArguments;

  public IRCall(String processName, List<LinearArgument> linearArguments, List<TypeArgument> typeArguments) {
    this.processName = processName;
    this.linearArguments = linearArguments;
    this.typeArguments = typeArguments;
  }

  public String getProcessName() {
    return processName;
  }

  public List<LinearArgument> getLinearArguments() {
    return linearArguments;
  }

  public List<TypeArgument> getTypeArguments() {
    return typeArguments;
  }

  @Override
  public void accept(IRVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "call(" + processName;
    for (LinearArgument arg : this.linearArguments) {
      str += ", " + arg.getSourceRecord() + " L-> " + arg.getTargetRecord();
    }
    for (TypeArgument arg : this.typeArguments) {
      if (arg.getSourceType().isPresent()) {
        str += ", " + arg.getSourceType().get() + " T-> " + arg.getTargetType();
        if (arg.getSourcePolarity()) {
          str += " (dual)";
        }
      } else {
        str += ", ";
        if (arg.getSourcePolarity()) {
          str += "write";
        } else {
          str += "read";
        }
        str += " T-> " + arg.getTargetType();
      }
    }
    return str + ")";
  }
}
