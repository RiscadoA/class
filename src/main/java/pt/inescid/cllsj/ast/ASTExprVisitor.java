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

  public void visit(ASTMod expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTEq expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTNEq expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTLt expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTGt expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTAnd expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTOr expr) {
    visit((ASTExpr) expr);
  }

  public void visit(ASTNot expr) {
    visit((ASTExpr) expr);
  }
}
