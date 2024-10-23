package pt.ulisboa.tecnico.cllsj;

public class RunProcError extends Exception {

    public String msg;

    public RunProcError(String _msg) { msg = _msg; }
}
