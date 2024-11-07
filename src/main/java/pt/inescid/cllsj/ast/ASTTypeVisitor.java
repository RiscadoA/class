package pt.inescid.cllsj.ast;

import pt.inescid.cllsj.ast.types.*;

public abstract class ASTTypeVisitor {
  // Catch all for types which do not have their own visit method
  public abstract void visit(ASTType type);

  public void visit(ASTBangT type) {
    visit((ASTType) type);
  }

  public void visit(ASTBotT type) {
    visit((ASTType) type);
  }

  public void visit(ASTCaseT type) {
    visit((ASTType) type);
  }

  public void visit(ASTCoRecT type) {
    visit((ASTType) type);
  }

  public void visit(ASTIdT type) {
    visit((ASTType) type);
  }

  public void visit(ASTNotT type) {
    visit((ASTType) type);
  }

  public void visit(ASTOfferT type) {
    visit((ASTType) type);
  }

  public void visit(ASTOneT type) {
    visit((ASTType) type);
  }

  public void visit(ASTRecT type) {
    visit((ASTType) type);
  }

  public void visit(ASTRecvT type) {
    visit((ASTType) type);
  }

  public void visit(ASTSendT type) {
    visit((ASTType) type);
  }

  public void visit(ASTWhyT type) {
    visit((ASTType) type);
  }
}
