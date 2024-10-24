package pt.inescid.cllsj;

public class LinSessionValue extends SessionField {
  LinSession lin;

  public LinSessionValue(String id) {
    lin = new LinSession(id);
  }

  public LinSession getLin() {
    return lin;
  }
}
