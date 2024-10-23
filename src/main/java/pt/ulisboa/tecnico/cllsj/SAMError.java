package pt.ulisboa.tecnico.cllsj;

public class SAMError extends Exception {
  public String msg;

  public SAMError(String _msg) {
    msg = _msg;
  }
}
