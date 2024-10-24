package pt.inescid.cllsj.compiler;

import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.types.ASTType;

public class SessionRecord {
  public ASTType type;
  public ASTNode continuation;

  public SessionRecord(ASTType type, ASTNode continuation) {
    this.type = type;
    this.continuation = continuation;
  }
}
