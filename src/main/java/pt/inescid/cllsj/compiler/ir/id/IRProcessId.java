package pt.inescid.cllsj.compiler.ir.id;

public class IRProcessId {
  private String name;

  public IRProcessId(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return name;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRProcessId other = (IRProcessId) obj;
    return name == other.name;
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }
}
