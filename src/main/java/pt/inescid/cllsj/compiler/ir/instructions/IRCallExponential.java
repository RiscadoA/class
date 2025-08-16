package pt.inescid.cllsj.compiler.ir.instructions;

import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;

public class IRCallExponential extends IRInstruction {
  private int exponential;
  private int argRecord;

  // Used when the reference count of the exponential should be decreased.
  // Enables the compiler to optimize memory management by using the original record instead of
  // cloning it when the reference count gets to zero.
  private boolean decRefCount;

  public IRCallExponential(int exponential, int argRecord, boolean decRefCount) {
    this.exponential = exponential;
    this.argRecord = argRecord;
    this.decRefCount = decRefCount;
  }

  public int getExponential() {
    return exponential;
  }

  public int getArgRecord() {
    return argRecord;
  }

  public boolean shouldDecreaseRefCount() {
    return decRefCount;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "callExponential("
        + exponential
        + ", "
        + argRecord
        + (decRefCount ? ", decRefCount" : "")
        + ")";
  }

  @Override
  public boolean usesRecord(int record) {
    return this.argRecord == record;
  }
}
