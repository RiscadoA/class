package pt.inescid.cllsj;

public class VInt extends Value {

  int val;

  public VInt(int val) {
    this.val = val;
  }

  public int get() {
    return val;
  }

  public String toStr() {
    return Integer.toString(val);
  }

  public boolean equal(Value v) {
    return val == ((VInt) v).get();
  }
}
