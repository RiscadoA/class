package pt.inescid.cllsj.compiler.ir.instructions_old;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRCallExponential extends IRInstruction {
  private int exponential;
  private IRType exponentialType;
  private int argRecord;

  public IRCallExponential(int exponential, IRType exponentialType, int argRecord) {
    this.exponential = exponential;
    this.exponentialType = exponentialType;
    this.argRecord = argRecord;
  }

  public int getExponential() {
    return exponential;
  }

  public IRType getExponentialType() {
    return exponentialType;
  }

  public int getArgRecord() {
    return argRecord;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "callExponential(" + exponential + "[" + exponentialType + "], " + argRecord + ")";
  }

  @Override
  public IRInstruction clone() {
    return new IRCallExponential(exponential, exponentialType, argRecord);
  }

  @Override
  public void renameRecords(Function<Integer, Integer> renamer) {
    argRecord = renamer.apply(argRecord);
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    exponential = renamer.apply(exponential);
  }

  @Override
  public void substituteTypes(Function<IRType, IRType> types) {
    exponentialType = types.apply(exponentialType);
  }
}
