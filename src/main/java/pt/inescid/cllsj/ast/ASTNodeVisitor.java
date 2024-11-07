package pt.inescid.cllsj.ast;

import pt.inescid.cllsj.ast.nodes.*;

public abstract class ASTNodeVisitor {
  // Catch all for nodes which do not have their own visit method
  public abstract void visit(ASTNode node);

  // Catch all for expression nodes which do not have their own visit method
  public void visit(ASTExpr node) {
    visit((ASTNode) node);
  }

  public void visit(ASTBang node) {
    visit((ASTNode) node);
  }

  public void visit(ASTCall node) {
    visit((ASTNode) node);
  }

  public void visit(ASTCase node) {
    visit((ASTNode) node);
  }

  public void visit(ASTClose node) {
    visit((ASTNode) node);
  }

  public void visit(ASTCoClose node) {
    visit((ASTNode) node);
  }

  public void visit(ASTCut node) {
    visit((ASTNode) node);
  }

  public void visit(ASTEmpty node) {
    visit((ASTNode) node);
  }

  public void visit(ASTFwd node) {
    visit((ASTNode) node);
  }

  public void visit(ASTId node) {
    visit((ASTNode) node);
  }

  public void visit(ASTMix node) {
    visit((ASTNode) node);
  }

  public void visit(ASTPrintLn node) {
    visit((ASTNode) node);
  }

  public void visit(ASTProcDef node) {
    visit((ASTNode) node);
  }

  public void visit(ASTProgram node) {
    visit((ASTNode) node);
  }

  public void visit(ASTRecv node) {
    visit((ASTNode) node);
  }

  public void visit(ASTSelect node) {
    visit((ASTNode) node);
  }

  public void visit(ASTSend node) {
    visit((ASTNode) node);
  }

  public void visit(ASTString node) {
    visit((ASTExpr) node);
  }

  public void visit(ASTVId node) {
    visit((ASTExpr) node);
  }

  public void visit(ASTWhy node) {
    visit((ASTNode) node);
  }
}
