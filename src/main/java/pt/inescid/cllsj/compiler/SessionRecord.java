package pt.inescid.cllsj.compiler;

import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.types.ASTType;

public class SessionRecord {
  public int size;
  public ASTType type;
  public ASTNode continuation;

  public SessionRecord(ASTType type, ASTNode continuation) {
    this.size = SizeCalculator.calculate(type);
    this.type = type;
    this.continuation = continuation;
  }

  public SessionRecord(SessionRecord other) {
    this(other.type, other.continuation);
  }
}
