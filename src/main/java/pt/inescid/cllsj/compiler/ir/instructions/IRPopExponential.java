package pt.inescid.cllsj.compiler.ir.instructions;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRPopExponential extends IRPop {
  private int argExponential;

  public IRPopExponential(int record, IRType recordType, int argExponential) {
    super(record, recordType);
    this.argExponential = argExponential;
  }

  public int getArgExponential() {
    return argExponential;
  }

  @Override
  public IRInstruction clone() {
    return new IRPopExponential(getRecord(), getRecordType(), argExponential);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return toString("popExponential", Integer.toString(argExponential));
  }

  @Override
  public void renameExponentials(Function<Integer, Integer> renamer) {
    argExponential = renamer.apply(argExponential);
  }
}
