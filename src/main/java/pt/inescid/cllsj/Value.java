package pt.inescid.cllsj;

public abstract class Value extends Server {

  public LinSession call(String _chi) throws Exception {
    LinSession session = new LinSession(_chi);
    Value linValue = this;
    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              session.send(linValue);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });
    return session;
  }

  public abstract String toStr();

  public abstract boolean equal(Value v);
}
