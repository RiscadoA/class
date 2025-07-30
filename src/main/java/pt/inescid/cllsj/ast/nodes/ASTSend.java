package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTSend extends ASTNode {

  String chs;
  String cho;
  ASTType type;
  ASTType tys_lhs;
  ASTType tys_rhs;
  ASTNode lhs;
  ASTNode rhs;

  public ASTSend(String _chs, String _cho, ASTType _type, ASTNode _lhs, ASTNode _rhs) {
    chs = _chs;
    cho = _cho;
    type = _type;
    lhs = _lhs;
    rhs = _rhs;
  }

  @Override
  public String getSubjectCh() {
    return chs;
  }

  public String getChs() {
    return chs;
  }

  public void setChs(String chs) {
    this.chs = chs;
  }

  public String getCho() {
    return cho;
  }

  public void setCho(String cho) {
    this.cho = cho;
  }

  public ASTType getLhsType() {
    return tys_lhs;
  }

  public ASTNode getLhs() {
    return lhs;
  }

  public ASTNode getRhs() {
    return rhs;
  }

  public ASTType getRhsType() {
    return tys_rhs;
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

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    if (_ch.equals(chs) && rhs == here) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.setrhs(t);
      pushUse.eg = eg;
      here.setanc(pushUse);
      pushUse.setanc(this);
      rhs = pushUse;
    } else if (_ch.equals(cho) && lhs == here) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.setrhs(t);
      pushUse.eg = eg;
      here.setanc(pushUse);
      pushUse.setanc(this);
      lhs = pushUse;
    } else anc.ASTInsertUse(_ch, t, this, disCont);
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;

    here.setanc(pushCall);
    pushCall.setanc(this);
    if (lhs == here) lhs = pushCall;
    else rhs = pushCall;
  }

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception {
    Set<String> s = lhs.fn(new HashSet<String>());
    if (!ch.equals(cho) && s.contains(ch)) {
      lhs = lhs.ASTweakeningOnLeaf(ch, typ, exp);
      return this;
    }
    rhs = rhs.ASTweakeningOnLeaf(ch, typ, exp);
    return this;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(chs) && rhs == here) {
      ASTNode pushWhy = new ASTWhy(_ch, _t, here);
      pushWhy.eg = eg;
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      eg.insert(_ch, _t);
      rhs = pushWhy;
    } else if (_ch.equals(cho) && lhs == here) {
      ASTNode pushWhy = new ASTWhy(_ch, _t, here);
      pushWhy.eg = eg;
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      eg.insert(_ch, _t);
      lhs = pushWhy;
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == lhs) lhs = newCont;
    else rhs = newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    this.eg = eg;
    // this.inferUses(chs,ed,ep);

    ASTType typee = null;
    ASTType ty = ed.find(chs);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, chs, ep);

    if (type != null) {
      typee = (type != null) ? type.unfoldType(ep) : type;
      //	    typee = ASTType.unfoldRec(typee);
    }
    if (ty instanceof ASTSendT) {
      ASTSendT tys = (ASTSendT) ty;

      // for SAM
      // offset = tys.GetOffset();
      type = tys;
      // System.out.println("SEND offset "+chs+"@"+offset);
      // was for SAM

      tys_lhs = tys.getlhs().unfoldType(ep);
      tys_rhs = tys.getrhs().unfoldType(ep);
      //	    tys_lhs = ASTType.unfoldRec(tys_lhs);
      //	    System.out.println(tys_lhs);
      //	    ASTType.unfoldRecInfer(tys_lhs, lhs, cho);

      ed.upd(chs, null);

      Env<ASTType> el = ed.assoc(cho, tys_lhs);

      if (typee != null && !typee.equalst(tys_lhs, ep, true, new Trail()))
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "SEND "
                + cho
                + " type mismatch: found="
                + tys_lhs.toStr(ep)
                + " declared="
                + typee.toStr(ep));

      if (lhs instanceof ASTExpr) {
        ASTExpr pe = (ASTExpr) lhs;
        try {
          lhs = compileExpr(cho, pe, tys_lhs, ep);
        } catch (Exception e) {
          if (pe instanceof ASTVId) { // expressio
            String choo = ((ASTVId) pe).ch;
            try { // check if the free output is of a linear or unrestricted name
              ed.find(choo);
              lhs = new ASTFwd(cho, choo);
            } catch (Exception ex) {
              lhs = new ASTFwdB(cho, choo);
            }
          } else
            throw new TypeError(
                "Line "
                    + lineno
                    + " :"
                    + "SEND "
                    + chs
                    + ": cannot be parsed as send of basic expression nor as free output.");
        }
        lhs.setanc(this);
      }

      ep = ASTNode.propagateRVar(ep, chs, cho);

      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

      lhs.typecheck(el, eglhs, ep);

      lhs.linclose(el, ep);

      lhs = ASTInferLinClose(lhs, cho, el, ep);

      ed.upd(chs, tys.getrhs().unfoldType(ep));

      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());

      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "SEND: "
              + chs
              + " is not of SEND type, found: "
              + ty.toStr(ep));
  }

  public Set<String> fn(Set<String> s) {
    s = lhs.fn(s);
    s.remove(cho);
    s = rhs.fn(s);
    s.add(chs);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = lhs.fnLinear(s);
    s.remove(cho);
    s = rhs.fnLinear(s);
    s.add(chs);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTSend p;
    if (type == null) p = new ASTSend(chs, cho, type, lhs.subst(e), rhs.subst(e));
    else p = new ASTSend(chs, cho, type.subst(e), lhs.subst(e), rhs.subst(e));
    p.lhs.setanc(p);
    p.rhs.setanc(p);
    p.lineno = lineno;
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chs) {
      chs = x;
      rhs.subs(x, y);
    } else if (x == cho) { // we rename the bound name chi to fresh to avoid capturing name x
      String fresh = ASTNode.gensym();
      lhs.subs(fresh, cho);
      cho = fresh;
      lhs.subs(x, y);
      rhs.subs(x, y);
    } else if (y != cho) {
      lhs.subs(x, y);
      rhs.subs(x, y);
    } else rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(chs);
    LinSession sessionOut = new LinSession(cho);

    CLLSj.inc_sends(+1);
    channel.send(sessionOut);
    CLLSj.inc_sends(-1);

    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              lhs.runproc(ep, ed.assoc(cho, sessionOut), eg, logger);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });

    /*	CLLSj.threadPool.submit(
    	new Runnable(){public void run(){
    	try{
    	rhs.runproc(ep, ed, eg, logger);
    	}catch (Exception e){ e.printStackTrace(System.out); }
    	}});

    	logger);
    */

    rhs.runproc(ep, ed, eg, logger);
  }

  public void show() {
    System.out.println(this + " " + anc);
    rhs.show();
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = frame.find(chs);
    ASTType argType = tys_lhs;
    int sessionSize = argType.SetOffsets(0, ep) + 1;

    if (sf instanceof LinSessionValue) {
      LinSessionValue lsv = (LinSessionValue) sf;
      Channel channel = lsv.getLin();
      SessionClosure clos = new SessionClosure(cho, sessionSize, argType.isPos(ep), lhs, frame, ep);
      channel.send(clos);
      if (CLLSj.trace) {
        System.out.println("send-op-lc " + chs + " " + channel + " " + rhs);
      }
      p_cont.code = rhs;
      p_cont.frame = frame;
      p_cont.epnm = ep;
    } else {

      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      ASTType contType = ((ASTSendT) type).getrhs().unfoldType(ep);

      if (CLLSj.trace) {
        System.out.println("send-op " + chs + " " + srec + " @ " + doffset);
      }

      SessionClosure clos = new SessionClosure(cho, sessionSize, argType.isPos(ep), lhs, frame, ep);
      if (srec.getPol()) {
        srec.writeSlot(clos, doffset);
        sref.incOffset();

        if (!tys_rhs.isPos(ep)) {
          ASTNode cont = srec.getCont();
          Env<SessionField> frm = srec.getFrame();
          Env<EnvEntry> epn = srec.getFrameP();
          boolean pold = srec.getPolDual();
          srec.setPolDual(srec.getPol()); // receive on other endpoint
          srec.setPol(pold);

          srec.setCont(rhs);
          srec.setcch(chs);
          srec.setFrame(frame);
          srec.setFrameP(ep);
          p_cont.code = cont;
          p_cont.frame = frm;
          p_cont.epnm = epn;

        } else {

          p_cont.code = rhs;
          p_cont.frame = frame;
          p_cont.epnm = ep;
        }
      } else {
        throw new SAMError("send-op - " + chs);
      }
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
