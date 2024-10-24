package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.nodes.ASTTypeDef;

public class TypeDefEntry implements EnvEntry {
  ASTTypeDef t;

  public TypeDefEntry(ASTTypeDef _t) {
    t = _t;
  }

  public ASTTypeDef getTypeDef() {
    return t;
  }
}
