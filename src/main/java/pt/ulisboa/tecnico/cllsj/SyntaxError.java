package pt.ulisboa.tecnico.cllsj;

public class SyntaxError extends Exception {
  public String msg;

  public SyntaxError(String _msg) {
    msg = _msg;
  }
}
