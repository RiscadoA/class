package pt.inescid.cllsj.compiler.ir.expression;

import java.util.function.Function;

import pt.inescid.cllsj.compiler.ir.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.IRExpressionVisitor;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;

public class IRRead extends IRExpression {
  private IRDataLocation location;
  private IRSlot slot;
  
  public IRRead(IRDataLocation location, IRSlot slot) {
    this.location = location;
    this.slot = slot;
  }
  
  public IRDataLocation getLocation() {
    return location;
  }
  
  @Override
  public IRSlot getSlot() {
    return slot;
  }
  
  @Override
  public void accept(IRExpressionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRExpression clone() {
    return new IRRead(location, slot);
  }
  
  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    location = replacer.apply(location);
  }
  
  @Override
  public String toString() {
    return "read(" + location + ", " + slot + ")";
  }
}
