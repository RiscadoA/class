package pt.inescid.cllsj.compiler.ir.id;

import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

public class IRLocalDataId {
  private int index;

  public IRLocalDataId(int index) {
    this.index = index;
  }

  public int getIndex() {
    return index;
  }

  public IRDataLocation getLocation() {
    return IRDataLocation.local(this, IRSlotSequence.EMPTY);
  }

  @Override
  public String toString() {
    return "d" + index;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRLocalDataId other = (IRLocalDataId) obj;
    return index == other.index;
  }

  @Override
  public int hashCode() {
    return Integer.hashCode(index);
  }
}
