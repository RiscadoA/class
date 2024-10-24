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
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.SessionValue;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.Value;

public class ASTPromoCoExpr extends ASTNode {

  String ch;
  ASTExpr expr;
  boolean promoted; // = true if calls to closures expanded

  public ASTPromoCoExpr(String _ch, ASTExpr _expr) {
    expr = _expr;
    ch = _ch;
    promoted = false;
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    expr = (ASTExpr) newCont;
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    anc.ASTInsertUse(ch, t, this, disCont); // insert above up
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    anc.ASTInsertCall(ch, cho, t, this); // insert above up
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    return this.ASTweakeningTerm(_ch, exp);
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    this.inferUses(ch, ed, ep);
    ASTType bt = expr.etypecheck(ed, eg, ep, false);

    ASTType idt = ed.find(ch);
    if (idt instanceof ASTBangT) {
      ASTType idtb = ((ASTBangT) idt).getin();
      if (idtb instanceof ASTBasicType) {
        if (!bt.equalst(idtb.dual(ep), ep, true, new Trail()))
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "LET "
                  + ch
                  + " type mismatch; found="
                  + bt.toStr(ep)
                  + " expected="
                  + idt.toStr(ep));
        ed.upd(ch, null);
      } else throw new TypeError("Line " + lineno + " :" + "LET " + ch + " not of !-basic type");
    } else throw new TypeError("Line " + lineno + " :" + "LET " + ch + " not of basic type");
  }

  public Set<String> fn(Set<String> s) {
    s = expr.fn(s);
    s.add(ch);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = expr.fnLinear(s);
    s.add(ch);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) ch = x;
    else expr.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Value v =
        expr.eval(
            ed, eg); // important to keep ed here because of InsertCall resolution in VId typecheck
    Channel channel = (Channel) ed.find(ch);
    channel.send(v);
  }

  static ASTType negtype = new ASTCointT();

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    if (!promoted) {
      Set<String> fns = new HashSet<String>();

      fns = expr.fn(fns);
      ASTNode base = new ASTPromoCoExpr(ch, expr);
      ((ASTPromoCoExpr) base).promoted = true;
      for (String id : fns) {
        SessionField sf = frame.find(id);
        if (sf instanceof SessionClosure) {
          base = new ASTCall(id, "$" + id, negtype, base); // any negative type will do
          System.out.println("SessionClosure added Call " + id);
        }
      }
      base.sam(frame, ep);

    } else {

      Value v = expr.sameval(frame);

      IndexedSessionRef sref = (IndexedSessionRef) frame.find(ch);
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();

      SessionValue sval = new SessionValue();
      sval.setValue(v);

      srec.writeSlot(sval, doffset);
      sref.incOffset();

      //	System.out.println("LET! "+ch+" := "+v+" "+sval+" @"+doffset+" "+sref.getOffset()+"
      // "+srec+" "+sref);

      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();

      //	System.out.println("LET frm ") ;
      //	frm.crawl();

      Env<EnvEntry> epn = srec.getFrameP();

      srec.setPol(false);

      //	sval.setFrame(frame);
      //	sval.setFrameP(ep);

      srec.setFrame(frame);
      srec.setFrameP(ep);

      cont.sam(frm, epn);
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    Value v = expr.sameval(frame);

    IndexedSessionRef sref = (IndexedSessionRef) frame.find(ch);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();

    if (CLLSj.trace) {
      System.out.println("copromoexpr-op " + ch + " " + srec + " @ " + doffset);
    }

    srec.writeSlot(v, doffset);
    sref.incOffset();

    //	System.out.println("LET! "+ch+" := "+v+" "+sval+" @"+doffset+" "+sref.getOffset()+" "+srec+"
    // "+sref);

    ASTNode cont = srec.getCont();
    Env<SessionField> frm = srec.getFrame();

    //	System.out.println("LET frm ") ;
    //	frm.crawl();

    Env<EnvEntry> epn = srec.getFrameP();

    srec.setPol(false);

    //	sval.setFrame(frame);
    //	sval.setFrameP(ep);

    srec.setFrame(frame);
    srec.setFrameP(ep);

    p_cont.code = cont;
    p_cont.frame = frm;
    p_cont.epnm = epn;

    //	    cont.sam(frm,epn);

  }
}
