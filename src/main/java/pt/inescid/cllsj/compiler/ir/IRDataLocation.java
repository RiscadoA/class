package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;

public class IRDataLocation {
  private int id; // Identifier for the data location
  private boolean remote; // True if the location is remote, false if local
  private IRSlotSequence offset; // How many slots to skip from the base location

  public static IRDataLocation local(int id, IRSlotSequence offset) {
    return new IRDataLocation(id, false, offset);
  }

  public static IRDataLocation remote(int id, IRSlotSequence offset) {
    return new IRDataLocation(id, true, offset);
  }

  private IRDataLocation(int id, boolean remote, IRSlotSequence offset) {
    this.id = id;
    this.remote = remote;
    this.offset = offset;
  }

  public int getId() {
    return id;
  }

  public boolean isRemote() {
    return remote;
  }

  public IRSlotSequence getOffset() {
    return offset;
  }

  @Override
  public String toString() {
    return (remote ? "remote" : "local") + "(" + id + ")[" + offset.toString() + "]";
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRDataLocation other = (IRDataLocation) obj;
    return id == other.id && remote == other.remote && offset.equals(other.offset);
  }

  @Override
  public int hashCode() {
    int result = Integer.hashCode(id);
    result = 31 * result + Boolean.hashCode(remote);
    result = 31 * result + offset.hashCode();
    return result;
  }
}
