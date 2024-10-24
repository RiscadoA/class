package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.nodes.ASTType;

public class TypeEntry implements EnvEntry {
  ASTType t;

  public TypeEntry(ASTType _t) {
    t = _t;
  }

  public ASTType getType() {
    return t;
  }
}
