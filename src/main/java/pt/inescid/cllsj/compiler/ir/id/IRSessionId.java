package pt.inescid.cllsj.compiler.ir.id;

public class IRSessionId {
  private int index;

  public IRSessionId(int index) {
    this.index = index;
  }

  public int getIndex() {
    return index;
  }

  public IRLocalDataId getLocalData() {
    return new IRLocalDataId(index);
  }

  @Override
  public String toString() {
    return "s" + index;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRSessionId other = (IRSessionId) obj;
    return index == other.index;
  }

  @Override
  public int hashCode() {
    return Integer.hashCode(index);
  }
}
