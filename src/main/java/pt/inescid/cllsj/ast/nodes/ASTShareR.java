package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageBLT;
import pt.inescid.cllsj.ast.types.ASTUsageBT;
import pt.inescid.cllsj.ast.types.ASTUsageLT;
import pt.inescid.cllsj.ast.types.ASTUsageT;

public class ASTShareR extends ASTNode {

  String sh;
  ASTNode lhs;
  ASTNode rhs;

  public ASTShareR(String _sh, ASTNode _lhs, ASTNode _rhs) {
    sh = _sh;
    lhs = _lhs;
    rhs = _rhs;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    if (from == lhs) {
      ASTNode nnode = f.apply(from);
      lhs.setanc(nnode);
      lhs = nnode;
      nnode.setanc(this);
    } else if (from == rhs) {
      ASTNode nnode = f.apply(from);
      rhs.setanc(nnode);
      rhs = nnode;
      nnode.setanc(this);
    } else {
      throw new Exception("ASTInsertPipe: call not expected");
    }
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    anc.ASTInsertUse(ch, t, this, disCont);
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;

    here.setanc(pushCall);
    pushCall.setanc(this);
    if (lhs == here) {
      lhs = pushCall;
    } else {
      rhs = pushCall;
    }
  }

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == lhs) lhs = newCont;
    else rhs = newCont;
  }

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception {
    Set<String> s = lhs.fn(new HashSet<String>());
    if (s.contains(ch)) {
      lhs = lhs.ASTweakeningOnLeaf(ch, typ, exp);
      return this;
    }
    rhs = rhs.ASTweakeningOnLeaf(ch, typ, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    ASTType ty = ed.find(sh);

    ty = ty.unfoldType(ep);
    if (ty instanceof ASTUsageLT) {
      ASTUsageLT tys = (ASTUsageLT) ty;
      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());
      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
      ed.upd(sh, new ASTUsageT(tys.getin()));
      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());
      lhs.typecheck(ed, eglhs, ep);
      lhs.linclose(ed, ep);
    } else if (ty instanceof ASTUsageBLT) {
      ASTUsageBLT tys = (ASTUsageBLT) ty;
      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());
      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
      ed.upd(sh, new ASTUsageBT(tys.getin()));
      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());
      lhs.typecheck(ed, eglhs, ep);
      lhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "SHARER: " + sh + " is neither of USAGEL nor of USAGE!L type.");
  }

  public Set<String> fn(Set<String> s) {
    s = lhs.fn(s);
    s = rhs.fn(s);
    s.add(sh);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = lhs.fnLinear(s);
    s = rhs.fnLinear(s);
    s.add(sh);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTShareR p = new ASTShareR(sh, lhs.subst(e), rhs.subst(e));
    p.lhs.setanc(p);
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == sh) sh = x;

    lhs.subs(x, y);
    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    LinSession session = (LinSession) ed.find(sh);

    session.incUsages(1);

    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              lhs.runproc(ep, ed, eg, logger);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });
    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              rhs.runproc(ep, ed, eg, logger);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });
  }
}
