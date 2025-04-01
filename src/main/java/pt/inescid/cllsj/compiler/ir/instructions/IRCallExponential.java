package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRCallExponential extends IRInstruction {
  private int exponential;
  private int argRecord;
  private IRType argType;

  public IRCallExponential(int exponential, int argRecord, IRType argType) {
    this.exponential = exponential;
    this.argRecord = argRecord;
    this.argType = argType;
  }

  public int getExponential() {
    return exponential;
  }

  public int getArgRecord() {
    return argRecord;
  }

  public IRType getArgType() {
    return argType;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "callExponential(" + exponential + ", " + argRecord + ", " + argType + ")";
  }
}
