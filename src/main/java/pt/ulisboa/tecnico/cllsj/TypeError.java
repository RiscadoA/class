package pt.ulisboa.tecnico.cllsj;

public class TypeError extends Exception {
  public String msg;

  public TypeError(String _msg) {
    msg = _msg;
  }
}
