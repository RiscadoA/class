package pt.inescid.cllsj.ast;

import pt.inescid.cllsj.ast.types.*;

public abstract class ASTTypeVisitor {
  public abstract void visit(ASTBangT type);

  public abstract void visit(ASTBotT type);

  public abstract void visit(ASTCaseT type);

  public abstract void visit(ASTCoRecT type);

  public abstract void visit(ASTIdT type);

  public abstract void visit(ASTNotT type);

  public abstract void visit(ASTOfferT type);

  public abstract void visit(ASTOneT type);

  public abstract void visit(ASTRecT type);

  public abstract void visit(ASTRecvT type);

  public abstract void visit(ASTSendT type);

  public abstract void visit(ASTWhyT type);

  public abstract void visit(ASTintT type);

  public abstract void visit(ASTCointT type);

  public abstract void visit(ASTLintT type);

  public abstract void visit(ASTLCointT type);

  public abstract void visit(ASTLboolT type);

  public abstract void visit(ASTCoLboolT type);

  public abstract void visit(ASTLstringT type);

  public abstract void visit(ASTCoLstringT type);

  public abstract void visit(ASTSendTT type);

  public abstract void visit(ASTRecvTT type);

  public abstract void visit(ASTAffineT type);

  public abstract void visit(ASTCoAffineT type);

  public abstract void visit(ASTCellT type);

  public abstract void visit(ASTUsageT type);

  public abstract void visit(ASTCellLT type);

  public abstract void visit(ASTUsageLT type);
}
