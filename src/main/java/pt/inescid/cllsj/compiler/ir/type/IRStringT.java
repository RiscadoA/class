package pt.inescid.cllsj.compiler.ir.type;

import java.util.Optional;
import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;

public class IRStringT extends IRType {
  @Override
  public void accept(IRTypeVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    return "string";
  }

  @Override
  public IRType removeLastSlot() {
    return new IRCloseT();
  }
}
