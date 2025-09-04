package pt.inescid.cllsj.compiler.ir.id;

public class IRDropId {
  private int index;

  public IRDropId(int index) {
    this.index = index;
  }

  public int getIndex() {
    return index;
  }

  @Override
  public String toString() {
    return "e" + index;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRDropId other = (IRDropId) obj;
    return index == other.index;
  }

  @Override
  public int hashCode() {
    return Integer.hashCode(index);
  }
}
