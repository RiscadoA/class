package pt.inescid.cllsj;

public class VBool extends Value {

  boolean val;

  public VBool(boolean val) {
    this.val = val;
  }

  public boolean get() {
    return val;
  }

  public String toStr() {
    return Boolean.toString(val);
  }

  public boolean equal(Value v) {
    return val == ((VBool) v).get();
  }
}
