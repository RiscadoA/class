package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTSelect extends ASTNode {

  String ch;
  String lab;
  ASTNode rhs;
  ASTType contType;

  public ASTSelect(String _ch, String _lab, ASTNode _rhs) {
    ch = _ch;
    lab = _lab;
    rhs = _rhs;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    if (from == rhs) {
      ASTNode nnode = f.apply(from);
      rhs.setanc(nnode);
      rhs = nnode;
      nnode.setanc(this);
    } else {
      throw new Exception("ASTInsertPipe: call not expected");
    }
  }

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    if (_ch.equals(ch)) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.setrhs(t);
      pushUse.eg = eg;
      here.setanc(pushUse);
      pushUse.setanc(this);
      rhs = pushUse;
    } else anc.ASTInsertUse(_ch, t, this, disCont);
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;
    here.setanc(pushCall);
    pushCall.setanc(this);
    rhs = pushCall;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(ch)) {
      ASTNode pushWhy = new ASTWhy(_ch, here);
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    rhs = rhs.ASTweakeningOnLeaf(_ch, t, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;
    this.inferUses(ch, ed, ep);

    ASTType ty = ed.find(ch);

    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, ch, ep);

    if (ty instanceof ASTCaseT) {

      ASTCaseT tyr = (ASTCaseT) ty;
      ASTType tcase = tyr.cases.get(lab);
      contType = tcase;
      if (tcase == null) {
        throw new TypeError("Line " + lineno + " :" + "case: " + lab + " is not a valid label.");
      }
      ed.upd(ch, tcase);
      rhs.typecheck(ed, eg, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "case: " + ch + " is not of CASE type: found: " + ty.toStr(ep));
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(ch);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTSelect p = new ASTSelect(ch, lab, rhs.subst(e));
    p.lineno = this.lineno;
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) ch = x;

    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(ch);
    //	System.out.println("[RUNSTATUS] Start OFFER on session "+ session.id);
    channel.send(lab);
    logger.info("CHOICE " + lab + " on session " + channel.getId());
    //	System.out.println("[RUNSTATUS] Finish OFFER on session "+ session.id);
    rhs.runproc(ep, ed, eg, logger);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = (SessionField) frame.find(ch);

    if (sf instanceof LinSessionValue) {
      LinSessionValue lsv = (LinSessionValue) sf;
      Channel channel = lsv.getLin();
      LabelSet clos = new LabelSet(lab); // avoid allocation?
      if (CLLSj.trace) {
        System.out.println("sel#-op-lc " + lab + " " + ch + " " + channel);
      }
      channel.send(clos);
      p_cont.code = rhs;
      p_cont.frame = frame;
      p_cont.epnm = ep;
    } else {

      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      boolean pol = srec.getPol();

      if (CLLSj.trace) {
        System.out.println("sel#-op " + lab + " " + ch + " " + srec + " @ " + doffset + " " + pol);
      }

      if (pol) {
        srec.writeSlot(new LabelSet(lab), doffset);
        sref.incOffset();

        if (!contType.isPos(ep)) {
          ASTNode cont = srec.getCont();
          Env<SessionField> frm = srec.getFrame();
          Env<EnvEntry> epn = srec.getFrameP();
          boolean pold = srec.getPolDual();
          srec.setPolDual(srec.getPol()); // receive on other endpoint
          srec.setPol(pold);

          srec.setCont(rhs);
          srec.setcch(ch);
          srec.setFrame(frame);
          srec.setFrameP(ep);
          p_cont.code = cont;
          p_cont.frame = frm;
          p_cont.epnm = epn;

        } else {

          p_cont.code = rhs;
          p_cont.frame = frame;
          p_cont.epnm = ep;
          return;
        }
      } else {
        throw new SAMError("sel#-op - " + ch + " " + srec);
      }
    }
  }

  public void show() {
    System.out.println(this + " " + lab);
    rhs.show();
  }
}
