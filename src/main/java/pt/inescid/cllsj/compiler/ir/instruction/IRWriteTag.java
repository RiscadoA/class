package pt.inescid.cllsj.compiler.ir.instruction;

import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class IRWriteTag extends IRWrite {
  private int tag;

  public IRWriteTag(IRDataLocation location, int tag) {
    super(location);
    this.tag = tag;
  }

  public int getTag() {
    return tag;
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public IRInstruction clone() {
    return new IRWriteTag(location, tag);
  }

  @Override
  public String toString() {
    return "writeTag(" + location + ", " + tag + ")";
  }
}
