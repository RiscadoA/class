package pt.inescid.cllsj.compiler.ir.id;

public class IRTypeId {
  private int index;

  public IRTypeId(int index) {
    this.index = index;
  }

  public int getIndex() {
    return index;
  }

  @Override
  public String toString() {
    return "t" + index;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRTypeId other = (IRTypeId) obj;
    return index == other.index;
  }

  @Override
  public int hashCode() {
    return Integer.hashCode(index);
  }
}
