package pt.inescid.cllsj.compiler.c;

public class CAddress {
  private String base;
  private CSize offset;

  public static CAddress of(String base) {
    return new CAddress(base, CSize.zero());
  }

  public static CAddress of(String base, CSize offset) {
    return new CAddress(base, offset);
  }

  private CAddress(String base, CSize offset) {
    this.base = base;
    this.offset = offset;
  }

  public String getBase() {
    return base;
  }

  public CSize getOffset() {
    return offset;
  }

  @Override
  public String toString() {
    if (offset.equals(CSize.zero())) {
      return base;
    } else {
      return base + " + " + offset;
    }
  }

  public CAddress offset(CSize delta) {
    return new CAddress(base, offset.add(delta));
  }

  public CAddress align(CAlignment alignment) {
    return new CAddress("(char*)ALIGN((uintptr_t)(" + this + "), " + alignment + ")", CSize.zero());
  }

  public String cast(String type) {
    return "((" + type + "*)(" + base + " + " + offset + "))";
  }

  public String deref(String type) {
    return "(*(" + type + "*)(" + base + " + " + offset + "))";
  }
}
