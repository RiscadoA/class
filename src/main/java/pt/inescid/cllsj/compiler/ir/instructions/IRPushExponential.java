package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.List;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRPushExponential extends IRInstruction {
  public static class InheritedExponential {
    private int sourceExponential;
    private int targetExponential;

    public InheritedExponential(int sourceExponential, int targetExponential) {
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

  private int record;
  private String processName;
  private List<InheritedExponential> inheritedExponentials;
  private boolean shouldExecute;

  public IRPushExponential(
      int record,
      String processName,
      List<InheritedExponential> inheritedExponentials,
      boolean shouldExecute) {
    this.record = record;
    this.processName = processName;
    this.inheritedExponentials = inheritedExponentials;
    this.shouldExecute = shouldExecute;
  }

  public int getRecord() {
    return record;
  }

  public String getProcessName() {
    return processName;
  }

  public List<InheritedExponential> getInheritedExponentials() {
    return inheritedExponentials;
  }

  public boolean shouldExecute() {
    return shouldExecute;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    String str = "pushExponential(" + record + ", " + processName + ", " + shouldExecute;
    for (InheritedExponential inheritedExponential : inheritedExponentials) {
      str +=
          ", "
              + inheritedExponential.getSourceExponential()
              + " -> "
              + inheritedExponential.getTargetExponential();
    }
    return str + ")";
  }
}
