package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.SAM;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;

// unfolded

public class ASTCut extends ASTNode {
  String id;
  ASTType type;
  ASTNode lhs;
  ASTNode rhs;
  public boolean con;

  public int sessionSize; // for SAM

  public ASTCut(String _id, ASTType _type, ASTNode _lhs, ASTNode _rhs) {
    id = _id;
    type = _type;
    lhs = _lhs;
    rhs = _rhs;
  }

  public void show() {
    System.out.println("CUT LHS " + this);
    lhs.show();
    System.out.println("CUT RHS " + this);
    rhs.show();
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

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception {
    if (ch.equals(id)) {
      return this.ASTweakeningHere(ch, exp);
    } else {
      Set<String> s = lhs.fn(new HashSet<String>());
      if (s.contains(ch)) {
        lhs = lhs.ASTweakeningOnLeaf(ch, typ, exp);
        return this;
      }
      rhs = rhs.ASTweakeningOnLeaf(ch, typ, exp);
      return this;
    }
  }

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    if (_ch.equals(id)) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.eg = eg;
      pushUse.setrhs(t);
      here.setanc(pushUse);
      pushUse.setanc(this);
      if (lhs == here) {
        lhs = pushUse;
      } else {
        rhs = pushUse;
      }
    } else anc.ASTInsertUse(_ch, t, this, disCont);
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

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {

    if (_ch.equals(id)) {
      ASTNode pushWhy = new ASTWhy(_ch, here);
      pushWhy.eg = eg;
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      eg.insert(_ch, _t);
      if (lhs == here) {
        lhs = pushWhy;
      } else {
        rhs = pushWhy;
      }
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == lhs) lhs = newCont;
    else rhs = newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    ASTType cutty = type.unfoldType(ep);
    ASTType dcutty = cutty.dual(ep);
    // System.out.println("CUT type "+cutty);
    // System.out.println("type "+dcutty);

    // for SAM
    sessionSize = 1 + cutty.SetOffsets(0, ep);
    dcutty.SetOffsets(0, ep);
    // for SAM END

    Env<ASTType> el = ed.assoc(id, dcutty);
    Env<ASTType> er = ed.assoc(id, cutty);

    Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());
    lhs.typecheck(el, eglhs, ep);
    lhs.linclose(el, ep);
    lhs = ASTInferLinClose(lhs, id, el, ep);

    Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());
    rhs.typecheck(er, egrhs, ep);
    rhs.linclose(er, ep);
    rhs = ASTInferLinClose(rhs, id, er, ep);
  }

  public Set<String> fn(Set<String> s) {
    s = lhs.fn(s);
    s = rhs.fn(s);
    s.remove(id);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = lhs.fnLinear(s);
    s = rhs.fnLinear(s);
    s.remove(id);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTCut p = new ASTCut(id, type.subst(e), lhs.subst(e), rhs.subst(e));
    p.con = con;
    p.lhs.setanc(p);
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (x == id) {
      String fresh = ASTNode.gensym();
      rhs.subs(fresh, id);
      lhs.subs(fresh, id);
      id = fresh;
      rhs.subs(x, y);
      lhs.subs(x, y);
    } else if (y != id) rhs.subs(x, y);
    lhs.subs(x, y);
  }

  /*
    Calls runproc on lhs and rhs in parallel with session_directory = session_directory, id -> new Session(id)
  */
  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    LinSession session = new LinSession(id);
    Env<LinSession> edCut = ed.assoc(id, session);

    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              lhs.runproc(ep, edCut, eg, logger);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });

    /*	CLLSj.threadPool.submit( new Runnable(){
    public void run(){ try {
    rhs.runproc(ep, edCut, eg, logger);
    } catch (Exception e) {e.printStackTrace(System.out);} }
    }); */

    rhs.runproc(ep, edCut, eg, logger);
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    return "cut {\n\t" + lhs.toStr(ep) + "\n\t|" + id + "|\n\t" + rhs.toStr(ep) + "\n}";
  }

  public void samCCut(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    LinSessionValue session = new LinSessionValue(id);
    Env<SessionField> edCut = frame.assoc(id, session);

    if (CLLSj.trace) {
      System.out.println("ccut-op " + id);
    }

    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              SAM.SAMloop(lhs, edCut, ep);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });
    p_cont.code = rhs;
    p_cont.frame = edCut;
    p_cont.epnm = ep;
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    type = type.unfoldType(ep);

    // represent SessionValues as normal 1-slot session records !
    // this requires refactoring on cut, send, receive

    if (con) {
      samCCut(frame, ep, p_cont);
    } else {
      SessionRecord srec = SessionRecord.newSessionRecord(sessionSize);
      IndexedSessionRef srefw = new IndexedSessionRef(0, srec);
      IndexedSessionRef srefr = new IndexedSessionRef(0, srec);

      if (CLLSj.trace) {
        System.out.println("cut-op " + id + " " + srec + " size=" + sessionSize);
      }

      Env<SessionField> fwrite = frame.assoc(id, srefw);
      Env<SessionField> fread = frame.assoc(id, srefr);
      if (type.isPos(ep)) { // rhs writer

        srec.setPol(true);
        srec.setPolDual(false);
        srec.setcch(id);
        srec.setCont(lhs);
        srec.setFrame(fread);
        srec.setFrameP(ep);

        p_cont.code = rhs;
        p_cont.frame = fwrite;
        p_cont.epnm = ep;

      } else { // lhs writer
        srec.setPol(true);
        srec.setPolDual(false);
        srec.setcch(id);
        srec.setCont(rhs);
        srec.setFrame(fread);
        srec.setFrameP(ep);

        p_cont.code = lhs;
        p_cont.frame = fwrite;
        p_cont.epnm = ep;
      }
    }
  }
}
