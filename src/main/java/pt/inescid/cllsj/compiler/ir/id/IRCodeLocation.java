package pt.inescid.cllsj.compiler.ir.id;

public class IRCodeLocation {
  private String label;

  public static IRCodeLocation entry() {
    return new IRCodeLocation("entry");
  }

  public static IRCodeLocation label(String label) {
    return new IRCodeLocation(label);
  }

  private IRCodeLocation(String label) {
    this.label = label;
  }

  public String getLabel() {
    return label;
  }

  @Override
  public String toString() {
    return label;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (obj == null || getClass() != obj.getClass()) return false;
    IRCodeLocation other = (IRCodeLocation) obj;
    return label.equals(other.label);
  }

  @Override
  public int hashCode() {
    return label.hashCode();
  }
}
