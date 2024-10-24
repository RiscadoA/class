package pt.inescid.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTCoExpr extends ASTNode {

  String ch;
  ASTExpr expr;

  public ASTCoExpr(String _ch, ASTExpr _expr) {
    expr = _expr;
    ch = _ch;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
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

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    return this.ASTweakeningTerm(_ch, exp);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    this.inferUses(ch, ed, ep);

    ASTType bt = expr.etypecheck(ed, eg, ep, true);

    ASTType idt = ed.find(ch);
    if (idt instanceof ASTBasicType) {
      ASTBasicType idtt = ((ASTBasicType) idt);
      if (!bt.equalst(idtt.dual(ep), ep, true, new Trail()))
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "LET "
                + ch
                + " type mismatch; found="
                + bt.toStr(ep)
                + " expected="
                + idtt.toStr(ep));
      ed.upd(ch, null);
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

  public String toStr(Env<EnvEntry> ep) throws Exception {
    return "let " + ch + " " + expr.toStr(ep);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Value v = expr.eval(ed, eg);
    Channel channel = (Channel) ed.find(ch);
    channel.send(v);
  }

  /* here */

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    Value v = expr.sameval(frame);

    SessionValue sval = (SessionValue) frame.find(ch);

    ASTNode cont = sval.getCont();
    Env<SessionField> frm = sval.getFrame();
    Env<EnvEntry> epn = sval.getFrameP();
    sval.setValue(v);
    sval.setFrame(frame);
    sval.setFrameP(ep);
    cont.sam(frm, epn);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    Value v = expr.sameval(frame);

    SessionField sf = frame.find(ch);

    if (sf instanceof IndexedSessionRef) {

      IndexedSessionRef sref = (IndexedSessionRef) frame.find(ch);
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();

      if (CLLSj.trace) {
        System.out.println("coexpr-op " + ch + " " + srec + " @ " + doffset);
      }

      srec.writeSlot(v, doffset);
      sref.incOffset();

      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();
      Env<EnvEntry> epn = srec.getFrameP();

      srec.setPol(false);
      srec.setPolDual(true); // ended (don't care)
      srec.setFrame(frame);
      srec.setFrameP(ep);

      p_cont.code = cont;
      p_cont.frame = frm;
      p_cont.epnm = epn;

    } else {
      int u = 0 / 0;
    }
  }
}
