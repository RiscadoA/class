package pt.ulisboa.tecnico.cllsj;

public class ProcEntry implements EnvEntry {
  ASTNode p;

  public ProcEntry(ASTNode _p) {
    p = _p;
  }

  public ASTNode getProc() {
    return p;
  }
}
