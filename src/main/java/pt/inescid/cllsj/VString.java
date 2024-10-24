package pt.inescid.cllsj;

public class VString extends Value {

  String val;

  public VString(String val) {
    this.val = val;
  }

  public String get() {
    return val;
  }

  public String toStr() {
    return val;
  }

  public boolean equal(Value v) {
    return val.equals(((VString) v).get());
  }
}
