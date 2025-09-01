package pt.inescid.cllsj.compiler.ir.id;

import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

public class IRDataLocation {
  private int index;
  private boolean remote; // True if the location is remote, false if local
  private IRSlotSequence offset; // How many slots to skip from the base location

  public static IRDataLocation local(IRLocalDataId id, IRSlotSequence offset) {
    return new IRDataLocation(id.getIndex(), false, offset);
  }

  public static IRDataLocation remote(IRSessionId id, IRSlotSequence offset) {
    return new IRDataLocation(id.getIndex(), true, offset);
  }

  private IRDataLocation(int index, boolean remote, IRSlotSequence offset) {
    this.index = index;
    this.remote = remote;
    this.offset = offset;
  }

  public IRLocalDataId getLocalDataId() {
    if (remote) {
      throw new IllegalStateException("Data location is remote");
    }
    return new IRLocalDataId(index);
  }

  public IRSessionId getSessionId() {
    if (!remote) {
      throw new IllegalStateException("Data location is local");
    }
    return new IRSessionId(index);
  }

  public boolean isRemote() {
    return remote;
  }

  public IRSlotSequence getOffset() {
    return offset;
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    if (remote) {
      b.append(new IRSessionId(index).toString());
    } else {
      b.append(new IRLocalDataId(index).toString());
    }
    b.append("[");
    b.append(offset.toString());
    b.append("]");
    return b.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRDataLocation other = (IRDataLocation) obj;
    return index == other.index && remote == other.remote && offset.equals(other.offset);
  }

  @Override
  public int hashCode() {
    int result = Integer.hashCode(index);
    result = 31 * result + Boolean.hashCode(remote);
    result = 31 * result + offset.hashCode();
    return result;
  }
}
