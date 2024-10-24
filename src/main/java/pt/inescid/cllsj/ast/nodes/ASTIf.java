package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.VBool;

public class ASTIf extends ASTNode {

  ASTExpr expr;
  ASTNode thenp;
  ASTNode elsep;
  boolean promoted;

  public ASTIf(ASTExpr _expr, ASTNode _thenp, ASTNode _elsep) {
    expr = _expr;
    thenp = _thenp;
    elsep = _elsep;
    promoted = false;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    if (from == thenp) {
      ASTNode nnode = f.apply(from);
      thenp.setanc(nnode);
      thenp = nnode;
      nnode.setanc(this);
    } else if (from == elsep) {
      ASTNode nnode = f.apply(from);
      elsep.setanc(nnode);
      elsep = nnode;
      nnode.setanc(this);
    } else {
      throw new Exception("ASTInsertPipe: call not expected");
    }
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == expr) expr = (ASTExpr) newCont;
    else if (caller == thenp) thenp = newCont;
    else elsep = newCont;
  }

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    if (expr == here) {
      anc.ASTInsertUse(_ch, t, this, disCont);
      // System.out.println("Inferring use in " + _ch);
    } else {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.setrhs(t);
      pushUse.eg = eg;
      here.setanc(pushUse);
      pushUse.setanc(this);
      if (thenp == here) thenp = pushUse;
      else elsep = pushUse;
    }
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    if (expr == here) anc.ASTInsertCall(ch, cho, t, this); // in expr, push up !
    else {
      ASTNode pushCall = new ASTCall(ch, cho, t, here);
      pushCall.eg = eg;

      here.setanc(pushCall);
      pushCall.setanc(this);
      if (thenp == here) thenp = pushCall;
      else elsep = pushCall;
    }
  }

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception {
    thenp = thenp.ASTweakeningOnLeaf(ch, typ, exp);
    elsep = elsep.ASTweakeningOnLeaf(ch, typ, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;
    ASTType et;
    boolean lin;

    try {
      lin = true;
      et = expr.etypecheck(ed, eg, ep, lin);
    } catch (Exception ee) {
      ee.printStackTrace(System.out);
      lin = false;
      et = expr.etypecheck(ed, eg, ep, lin);
    }

    //	Env<ASTType> egb = eg.dupe();

    Env<ASTType> egb = eg.assoc("$DUM", new ASTBotT());

    //	egb.crawl();

    if (et instanceof ASTCoLboolT) {

      Env<ASTType> eb = ed.dup();

      thenp.typecheck(ed, egb, ep);

      //	egb.crawl();

      ed.updmove(eb); // correction for inference

      this.linclose(ed, ep);

      egb = eg.assoc("$DUM", new ASTBotT());

      elsep.typecheck(eb, egb, ep);

      //	egb.crawl();

      this.linclose(eb, ep);

      if (!eb.eq(ed))
        throw new TypeError(
            "Line " + lineno + " :" + "IF : unmatched then and else linear contexts");
    } else
      throw new TypeError("Line " + lineno + " :" + "IF : condition not of linear boolean type");
  }

  public Set<String> fn(Set<String> s) {
    s = expr.fn(s);
    s = thenp.fn(s);
    s = elsep.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = expr.fnLinear(s);
    s = thenp.fnLinear(s);
    s = elsep.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTIf p = new ASTIf(expr, thenp.subst(e), elsep.subst(e));
    p.thenp.setanc(p);
    p.elsep.setanc(p);
    p.expr.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    expr.subs(x, y);
    thenp.subs(x, y);
    elsep.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    VBool v = (VBool) expr.eval(ed, eg);
    if (v.get()) thenp.runproc(ep, ed, eg, logger);
    else elsep.runproc(ep, ed, eg, logger);
  }

  static ASTType negtype = new ASTCointT();

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    if (!promoted) {
      Set<String> fns = new HashSet<String>();

      fns = expr.fn(fns);
      ASTNode base = new ASTIf(expr, thenp, elsep);
      ((ASTIf) base).promoted = true;
      for (String id : fns) {
        SessionField sf = frame.find(id);
        if (sf instanceof SessionClosure) {
          base = new ASTCall(id, "$" + id, negtype, base); // any negative type will do
          System.out.println("SessionClosure added Call " + id);
        }
      }
      base.sam(frame, ep);

    } else {

      VBool v = (VBool) expr.sameval(frame);

      if (v.get()) thenp.sam(frame, ep);
      else elsep.sam(frame, ep);
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    if (false && !promoted) {
      Set<String> fns = new HashSet<String>();

      fns = expr.fn(fns);
      ASTNode base = new ASTIf(expr, thenp, elsep);
      ((ASTIf) base).promoted = true;
      for (String id : fns) {
        SessionField sf = frame.find(id);
        if (sf instanceof SessionClosure) {
          base = new ASTCall(id, "$" + id, negtype, base); // any negative type will do
          System.out.println("SessionClosure added Call " + id);
        }
      }
      p_cont.code = base;
      p_cont.frame = frame;
      p_cont.epnm = ep;
      //	    base.sam(frame,ep);

    } else {

      VBool v = (VBool) expr.sameval(frame);

      if (v.get()) {
        p_cont.code = thenp;
        p_cont.frame = frame;
        p_cont.epnm = ep;
        //		thenp.sam(frame,ep);
      } else {
        p_cont.code = elsep;
        p_cont.frame = frame;
        p_cont.epnm = ep;
        //		elsep.sam(frame,ep);
      }
    }
  }
}
