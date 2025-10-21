package pt.inescid.cllsj.ast;

import pt.inescid.cllsj.ast.nodes.*;

public abstract class ASTExprVisitor {

  public abstract void visit(ASTInt expr);

  public abstract void visit(ASTBool expr);

  public abstract void visit(ASTString expr);

  public abstract void visit(ASTVId expr);

  public abstract void visit(ASTAdd expr);

  public abstract void visit(ASTSub expr);

  public abstract void visit(ASTMul expr);

  public abstract void visit(ASTDiv expr);

  public abstract void visit(ASTMod expr);

  public abstract void visit(ASTEq expr);

  public abstract void visit(ASTNEq expr);

  public abstract void visit(ASTLt expr);

  public abstract void visit(ASTLeq expr);

  public abstract void visit(ASTGt expr);

  public abstract void visit(ASTGeq expr);

  public abstract void visit(ASTAnd expr);

  public abstract void visit(ASTOr expr);

  public abstract void visit(ASTNot expr);

  public abstract void visit(ASTOrd expr);
}
