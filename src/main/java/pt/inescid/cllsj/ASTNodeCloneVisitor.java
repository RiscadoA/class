package pt.inescid.cllsj;

import java.util.ArrayList;
import java.util.List;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.*;

public class ASTNodeCloneVisitor extends ASTNodeVisitor {

  ASTNode clo;
  ASTExprCloneVisitor ev;

  ASTNodeCloneVisitor() {
    ev = new ASTExprCloneVisitor();
  }

  public ASTNode getResult() {
    return clo;
  }

  // ============================ Instruction generation visit methods ============================

  @Override
  public void visit(ASTBang node) {
    node.getRhs().accept(this);
    clo = new ASTBang(node.getChr(), node.getChi(), node.getType(), clo);
  }

  @Override
  public void visit(ASTCall node) {
    node.getRhs().accept(this);
    clo = new ASTCall(node.getChr(), node.getChi(), node.getType(), clo);
  }

  @Override
  public void visit(ASTCase node) {
    ASTCase cs = new ASTCase(node.getCh());
    for (int i = 0; i < node.getCaseCount(); ++i) {
      String label = node.getCaseLabelFromIndex(i);
      ASTNode c = node.getCase(label);
      c.accept(this);
      try {
        cs.addCase(label, clo);
      } catch (Exception _e) {
      }
    }
  }

  @Override
  public void visit(ASTClose node) {
    clo = new ASTClose(node.getCh());
  }

  @Override
  public void visit(ASTCoClose node) {
    node.getRhs().accept(this);
    clo = new ASTCoClose(node.getCh(), clo);
  }

  @Override
  public void visit(ASTCut node) {
    node.getLhs().accept(this);
    ASTNode lhs = clo;
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTCut(node.getCh(), node.getChType(), lhs, rhs);
  }

  @Override
  public void visit(ASTEmpty node) {
    clo = node;
  }

  @Override
  public void visit(ASTFwd node) {
    clo = new ASTFwd(node.getCh1(), node.getCh2());
  }

  @Override
  public void visit(ASTFwdB node) {
    clo = new ASTFwdB(node.getCh1(), node.getCh2());
  }

  @Override
  public void visit(ASTId node) {
    ASTId idn = new ASTId(node.getId());
    List<ASTExpr> exprs = cloneListExpr(node.getExprs());
    List<ASTExpr> gexprs = cloneListExpr(node.getGExprs());
    List<ASTType> tvars = node.getTPars();
    idn.setExprs(exprs);
    idn.setGExprs(gexprs);
    idn.setTPars(tvars);
    clo = idn;
  }

  @Override
  public void visit(ASTMix node) {
    node.getLhs().accept(this);
    ASTNode lhs = clo;
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTMix(node.isConcurrent(), lhs, rhs);
  }

  @Override
  public void visit(ASTPrintLn node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    node.getExpr().accept(ev);
    clo = new ASTPrintLn(ev.getResult(), rhs, node.withNewLine());
  }

  @Override
  public void visit(ASTProcDef node) {}

  @Override
  public void visit(ASTProgram node) {}

  @Override
  public void visit(ASTRecv node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTRecv(node.getChr(), node.getChi(), node.getChiType(), rhs);
  }

  @Override
  public void visit(ASTSelect node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTSelect(node.getCh(), node.getLabel(), rhs);
  }

  @Override
  public void visit(ASTSend node) {
    ASTNode lhs = node.getLhs();
    if (lhs instanceof ASTExpr) {
      ((ASTExpr) lhs).accept(ev);
      lhs = ev.getResult();
    } else {
      lhs.accept(this);
      lhs = clo;
    }
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTSend(node.getChs(), node.getCho(), node.getLhsType(), lhs, rhs);
  }

  @Override
  public void visit(ASTUnfold node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTUnfold(node.getCh(), rhs);
  }

  @Override
  public void visit(ASTWhy node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTWhy(node.getCh(), rhs);
  }

  @Override
  public void visit(ASTCoExpr node) {
    node.getExpr().accept(ev);
    ASTExpr e = ev.getResult();
    clo = new ASTCoExpr(node.getCh(), e);
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    node.getExpr().accept(ev);
    ASTExpr e = ev.getResult();
    clo = new ASTPromoCoExpr(node.getCh(), e);
  }

  @Override
  public void visit(ASTIf node) {
    node.getThen().accept(this);
    ASTNode thenb = clo;
    node.getElse().accept(this);
    ASTNode elseb = clo;
    node.getExpr().accept(ev);
    ASTExpr e = ev.getResult();
    clo = new ASTIf(e, thenb, elseb);
  }

  @Override
  public void visit(ASTSendTy node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTSendTy(node.getChs(), node.getType(), rhs);
  }

  @Override
  public void visit(ASTRecvTy node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTRecvTy(node.getChs(), node.getTyid(), rhs);
  }

  @Override
  public void visit(ASTAffine node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTAffine(node.getCh(), rhs);
  }

  @Override
  public void visit(ASTUse node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTUse(node.getCh(), rhs);
  }

  @Override
  public void visit(ASTDiscard node) {
    clo = new ASTDiscard(node.getCh());
  }

  @Override
  public void visit(ASTCell node) {
    ASTNode rhs = node.getRhs();
    if (rhs instanceof ASTExpr) {
      ((ASTExpr) rhs).accept(ev);
      rhs = ev.getResult();
    } else {
      rhs.accept(this);
      rhs = clo;
    }
    clo = new ASTCell(node.getCh(), node.getChc(), node.getTypeRhs(), rhs);
  }

  @Override
  public void visit(ASTPut node) {
    ASTNode lhs = node.getLhs();
    if (lhs instanceof ASTExpr) {
      ((ASTExpr) lhs).accept(ev);
      lhs = ev.getResult();
    } else {
      lhs.accept(this);
      lhs = clo;
    }
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTPut(node.getChs(), node.getCho(), node.getLhsType(), lhs, rhs);
  }

  @Override
  public void visit(ASTTake node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTTake(node.getChr(), node.getChi(), node.getChiType(), rhs);
  }

  @Override
  public void visit(ASTRelease node) {
    clo = new ASTRelease(node.getChr());
  }

  @Override
  public void visit(ASTShare node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    node.getLhs().accept(this);
    ASTNode lhs = clo;
    clo = new ASTShare(node.getCh(), lhs, rhs, node.isConcurrent());
  }

  @Override
  public void visit(ASTShareL node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    node.getLhs().accept(this);
    ASTNode lhs = clo;
    clo = new ASTShareL(node.getCh(), lhs, rhs);
  }

  @Override
  public void visit(ASTShareR node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    node.getLhs().accept(this);
    ASTNode lhs = clo;
    clo = new ASTShareR(node.getCh(), lhs, rhs);
  }

  @Override
  public void visit(ASTScan node) {
    clo = new ASTScan(node.getCh(), node.getType(), node.isScanc());
  }

  @Override
  public void visit(ASTSleep node) {
    node.getRhs().accept(this);
    ASTNode rhs = clo;
    clo = new ASTSleep(node.getMsecs(), rhs);
  }

  @Override
  public void visit(ASTUnreachable node) {
    clo = new ASTUnreachable(node.getCh());
  }

  @Override
  public void visit(ASTExpr node) {
    throw new UnsupportedOperationException("Expressions should not appear here");
  }

  @Override
  public void visit(ASTCLLType node) {
    node.getRhs().accept(this);
  }

  // helper methods

  List<ASTExpr> cloneListExpr(List<ASTExpr> le) {
    List<ASTExpr> lr = new ArrayList<ASTExpr>();
    for (ASTExpr elt : le) {
      elt.accept(ev);
      lr.add(ev.getResult());
    }
    return lr;
  }
}
