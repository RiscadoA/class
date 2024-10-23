package pt.ulisboa.tecnico.cllsj;

class VString extends Value {

  String val;

  VString(String val) {
    this.val = val;
  }

  String get() {
    return val;
  }

  String toStr() {
    return val;
  }

  boolean equal(Value v) {
    return val.equals(((VString) v).get());
  }
}
