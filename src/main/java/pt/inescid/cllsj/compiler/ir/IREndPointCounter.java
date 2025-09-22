package pt.inescid.cllsj.compiler.ir;

import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.compiler.Compiler;

public class IREndPointCounter extends ASTNodeVisitor {
  private Compiler compiler;
  private IREnvironment env;
  private int count = 0;

  public static int count(Compiler compiler, IREnvironment env, ASTNode node) {
    IREndPointCounter counter = new IREndPointCounter();
    counter.compiler = compiler;
    counter.env = env;
    node.accept(counter);
    return counter.count;
  }

  @Override
  public void visit(ASTCut node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTMix node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTShare node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTEmpty node) {
    count += 1;
  }

  @Override
  public void visit(ASTClose node) {
    count += 1;
  }

  @Override
  public void visit(ASTId node) {
    count += 1;
  }

  @Override
  public void visit(ASTFwd node) {
    count += 1;
  }

  @Override
  public void visit(ASTFwdB node) {
    count += 1;
  }

  @Override
  public void visit(ASTCoExpr node) {
    count += 1;
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    count += 1;
  }

  @Override
  public void visit(ASTCoClose node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSelect node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCase node) {
    int oldCount = count;
    int maxCount = 0;
    for (int i = 0; i < node.getCaseCount(); ++i) {
      count = 0;
      node.getCase(node.getCaseLabelFromIndex(i)).accept(this);
      maxCount = Math.max(maxCount, count);
    }
    count = oldCount + maxCount;
  }

  @Override
  public void visit(ASTSend node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRecv node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTPrintLn node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTScan node) {
    count += 1;
  }

  @Override
  public void visit(ASTUnfold node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCall node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTBang node) {
    if (IRIsCloneableChecker.check(compiler, env, node.getType(), true).isPossible()) {
      node.getRhs().accept(this);
    } else {
      count += 1;
    }
  }

  @Override
  public void visit(ASTWhy node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTIf node) {
    int oldCount = count;
    count = 0;
    node.getThen().accept(this);
    int maxCount = count;
    count = 0;
    node.getElse().accept(this);
    maxCount = Math.max(maxCount, count);
    count = oldCount + maxCount;
  }

  @Override
  public void visit(ASTSendTy node) {
    count += 1;
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRecvTy node) {
    count += 1;
  }

  @Override
  public void visit(ASTAffine node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTUse node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTDiscard node) {
    count += 1;
  }

  @Override
  public void visit(ASTCell node) {
    count += 1;
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTPut node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTTake node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRelease node) {
    count += 1;
  }

  @Override
  public void visit(ASTSleep node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTUnreachable node) {
    count += 1;
  }

  @Override
  public void visit(ASTShareR node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTShareL node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTProgram node) {}

  @Override
  public void visit(ASTProcDef node) {}

  @Override
  public void visit(ASTExpr node) {}

  @Override
  public void visit(ASTCLLType node) {
    node.getRhs().accept(this);
  }
}
