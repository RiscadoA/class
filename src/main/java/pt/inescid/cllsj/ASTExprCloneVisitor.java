package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.nodes.*;

public class ASTExprCloneVisitor extends ASTExprVisitor {

  ASTExpr clo;

  ASTExprCloneVisitor() {}

  public ASTExpr getResult() {
    return clo;
  }

  // ============================ Expr Clone visit methods ============================

  public void visit(ASTInt expr) {
    clo = new ASTInt(expr.getValue());
  }

  public void visit(ASTBool expr) {
    clo = new ASTBool(expr.getValue());
  }

  public void visit(ASTString expr) {
    clo = new ASTString(expr.getValue());
  }

  public void visit(ASTVId expr) {
    clo = new ASTVId(expr.getCh());
  }

  public void visit(ASTAdd expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTAdd(lhs, rhs);
  }

  public void visit(ASTSub expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTSub(lhs, rhs);
  }

  public void visit(ASTMul expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTMul(lhs, rhs);
  }

  public void visit(ASTDiv expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTDiv(lhs, rhs);
  }

  public void visit(ASTMod expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTMod(lhs, rhs);
  }

  public void visit(ASTEq expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTEq(lhs, rhs);
  }

  public void visit(ASTNEq expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTNEq(lhs, rhs);
  }

  public void visit(ASTLt expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTLt(lhs, rhs);
  }

  public void visit(ASTLeq expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTLeq(lhs, rhs);
  }

  public void visit(ASTGt expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTGt(lhs, rhs);
  }

  public void visit(ASTGeq expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTGeq(lhs, rhs);
  }

  public void visit(ASTAnd expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTAnd(lhs, rhs);
  }

  public void visit(ASTOr expr) {
    expr.getLhs().accept(this);
    ASTExpr lhs = clo;
    expr.getRhs().accept(this);
    ASTExpr rhs = clo;
    clo = new ASTOr(lhs, rhs);
  }

  public void visit(ASTNot expr) {
    expr.getExpr().accept(this);
    ASTExpr e = clo;
    clo = new ASTNot(e);
  }

  public void visit(ASTOrd expr) {
    expr.getExpr().accept(this);
    ASTExpr e = clo;
    clo = new ASTOrd(e);
  }
}
