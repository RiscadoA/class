package pt.inescid.cllsj.ast;

import pt.inescid.cllsj.ast.nodes.*;

public abstract class ASTNodeVisitor {
  public abstract void visit(ASTBang node);

  public abstract void visit(ASTCall node);

  public abstract void visit(ASTCase node);

  public abstract void visit(ASTClose node);

  public abstract void visit(ASTCoClose node);

  public abstract void visit(ASTCut node);

  public abstract void visit(ASTEmpty node);

  public abstract void visit(ASTExpr node);

  public abstract void visit(ASTFwd node);

  public abstract void visit(ASTFwdB node);

  public abstract void visit(ASTId node);

  public abstract void visit(ASTMix node);

  public abstract void visit(ASTPrintLn node);

  public abstract void visit(ASTProcDef node);

  public abstract void visit(ASTProgram node);

  public abstract void visit(ASTRecv node);

  public abstract void visit(ASTSelect node);

  public abstract void visit(ASTSend node);

  public abstract void visit(ASTUnfold node);

  public abstract void visit(ASTWhy node);

  public abstract void visit(ASTCoExpr node);

  public abstract void visit(ASTPromoCoExpr node);

  public abstract void visit(ASTIf node);

  public abstract void visit(ASTSendTy node);

  public abstract void visit(ASTRecvTy node);

  public abstract void visit(ASTAffine node);

  public abstract void visit(ASTUse node);

  public abstract void visit(ASTDiscard node);

  public abstract void visit(ASTCell node);

  public abstract void visit(ASTPut node);

  public abstract void visit(ASTTake node);

  public abstract void visit(ASTRelease node);

  public abstract void visit(ASTShare node);

  public abstract void visit(ASTShareL node);

  public abstract void visit(ASTShareR node);

  public abstract void visit(ASTScan node);

  public abstract void visit(ASTSleep node);

  public abstract void visit(ASTUnreachable node);
}
