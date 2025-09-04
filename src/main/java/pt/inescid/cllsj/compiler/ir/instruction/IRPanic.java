package pt.inescid.cllsj.compiler.ir.instruction;

public class IRPanic extends IRInstruction {
  private String message;

  public IRPanic(String message) {
    this.message = message;
  }

  public String getMessage() {
    return message;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRPanic(message);
  }

  @Override
  public String toString() {
    return "panic(\"" + message + "\")";
  }
}
