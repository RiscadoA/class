package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.nodes.ASTNode;

public class ProcEntry implements EnvEntry {
  ASTNode p;

  public ProcEntry(ASTNode _p) {
    p = _p;
  }

  public ASTNode getProc() {
    return p;
  }
}
