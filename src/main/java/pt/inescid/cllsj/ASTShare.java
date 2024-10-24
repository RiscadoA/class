package pt.inescid.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTShare extends ASTNode {

  String sh;
  ASTNode lhs;
  ASTNode rhs;
  boolean con;

  public ASTShare(String _sh, ASTNode _lhs, ASTNode _rhs, boolean c) {
    sh = _sh;
    lhs = _lhs;
    rhs = _rhs;
    con = c;
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    anc.ASTInsertUse(ch, t, this, disCont);
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
    this.inferUses(sh, ed, ep);

    ASTType ty = ed.find(sh);

    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);
    if (ty instanceof ASTUsageT) {
      ASTUsageT tys = (ASTUsageT) ty;
      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());
      lhs.typecheck(ed, eglhs, ep);
      lhs.linclose(ed, ep);
      ed.upd(sh, ty);
      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());
      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
    } else if (ty instanceof ASTUsageBT) {
      ASTUsageBT tys = (ASTUsageBT) ty;
      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());
      lhs.typecheck(ed, eglhs, ep);
      lhs.linclose(ed, ep);
      ed.upd(sh, ty);
      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());
      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "SHARE: " + sh + " is neither of USAGE nor of USAGE! type.");
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
    ASTShare p = new ASTShare(sh, lhs.subst(e), rhs.subst(e), con);
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

    rhs.runproc(ep, ed, eg, logger);
  }

  public void samLCS(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              SAM.SAMloop(lhs, frame, ep);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });
    p_cont.code = rhs;
    p_cont.frame = frame;
    p_cont.epnm = ep;
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    if (con) {
      SessionField sf = frame.find(sh);
      if (sf instanceof MVar) {
        MVar v = (MVar) sf;
        v.increfc();
        samLCS(frame, ep, p_cont);
        return;
      } else {
        IndexedSessionRef sref = (IndexedSessionRef) sf;
        int doffset = sref.getOffset();
        SessionRecord srec = sref.getSessionRec();
        if (!srec.getPol()) {
          MVar v = (MVar) srec.readSlot(doffset);
          if (CLLSj.trace) {
            System.out.println("share-op " + sh + " " + srec + " @ " + doffset + " " + v);
          }
          sref.incOffset();
          p_cont.code = this;
          v.increfc();
          samLCS(frame.assoc(sh, v), ep, p_cont);
        } else throw new SAMError("share-op - " + sh);
      }
    } else {
      /* sequentialize */
      SAM.SAMloop(lhs, frame, ep);
      p_cont.code = rhs;
      p_cont.frame = frame;
      p_cont.epnm = ep;
    }
  }
}
