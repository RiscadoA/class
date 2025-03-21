package pt.inescid.cllsj.ast;

import pt.inescid.cllsj.ast.nodes.*;

public abstract class ASTExprVisitor {
  // Catch all for expressions which do not have their own visit method
  public abstract void visit(ASTExpr expr);

  public void visit(ASTInt expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTBool expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTString expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTVId expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTAdd expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTSub expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTMul expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTDiv expr) {
    visit((ASTExpr) expr);
  }
}
