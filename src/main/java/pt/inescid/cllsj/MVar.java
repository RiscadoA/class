package pt.inescid.cllsj;

public class MVar extends SessionField {

  static MVar mvar_free = null;
  MVar next_free = null;
  boolean empty = false;
  SessionClosure store;
  int refc = 1;

  public static MVar newMVar() {
    if (mvar_free == null) {
      return new MVar();
    } else {
      MVar mc = mvar_free;
      mvar_free = mc.next_free;
      mc.next_free = null;
      mc.refc = 1;
      return mc;
    }
  }

  public static synchronized void dropMVar(MVar sr) throws Exception {
    if (sr.refc == 0) throw new SAMError("drop free reference");
    sr.refc--;
    if (sr.refc == 0) {
      if (CLLSj.trace) {
        System.out.println("dropMVar " + sr);
      }
      sr.next_free = mvar_free;
      mvar_free = sr;
    }
  }

  public synchronized void increfc() throws Exception {
    refc++;
  }

  public void set(SessionClosure val) {
    store = val;
    empty = false;
  }

  public synchronized SessionClosure take() throws Exception {
    while (empty) this.wait();
    empty = true;
    SessionClosure v = store;
    store = null;
    return v;
  }

  public synchronized void put(SessionClosure val) {
    store = val;
    empty = false;
    notifyAll();
  }
}
