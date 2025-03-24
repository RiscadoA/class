package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRVarT;

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

  public static class TypeArgument {
    private IRType sourceType;
    private int targetType;

    // If sourceType is a IRVar, the variable's polarity will be xor'ed with this value.
    // Otherwise, this determines whether the argument is positive or negative.
    private boolean isDual;

    public TypeArgument(IRType sourceType, int targetType, boolean isDual) {
      this.sourceType = sourceType;
      this.targetType = targetType;
      this.isDual = isDual;
    }

    public IRType getSourceType() {
      return sourceType;
    }

    public int getTargetType() {
      return targetType;
    }

    public boolean isDual() {
      return isDual;
    }

    public boolean isPositive() {
      assert !(sourceType instanceof IRVarT);
      return isDual;
    }
  }

  private String processName;
  private List<LinearArgument> linearArguments;

  public IRCallProcess(String processName, List<LinearArgument> linearArguments) {
    this.processName = processName;
    this.linearArguments = linearArguments;
  }

  public String getProcessName() {
    return processName;
  }

  public List<LinearArgument> getLinearArguments() {
    return linearArguments;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "call(" + this.processName;
    for (LinearArgument arg : this.linearArguments) {
      str += ", " + arg.getSourceRecord() + " -> " + arg.getTargetRecord();
    }
    return str + ")";
  }
}
