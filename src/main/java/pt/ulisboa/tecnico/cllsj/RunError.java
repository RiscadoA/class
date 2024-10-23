package pt.ulisboa.tecnico.cllsj;

public class RunError extends Exception {
  public String msg;

  public RunError(String _msg) {
    msg = _msg;
  }
}
