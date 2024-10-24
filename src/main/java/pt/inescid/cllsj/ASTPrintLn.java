package pt.inescid.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTPrintLn extends ASTNode {

  ASTExpr expr;
  ASTNode rhs;
  boolean nl, promoted;
  static long time = 0;

  public ASTPrintLn(ASTExpr _expr, ASTNode _rhs, boolean _nl) {
    expr = _expr;
    rhs = _rhs;
    nl = _nl;
    promoted = false;
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

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == expr) expr = (ASTExpr) newCont;
    else rhs = newCont;
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    anc.ASTInsertUse(ch, t, this, disCont); // insert above up
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    anc.ASTInsertCall(ch, cho, t, this); // insert above up
  }

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    rhs = rhs.ASTweakeningOnLeaf(_ch, t, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    ASTType et;

    try {
      et = expr.etypecheck(ed, eg, ep, false);
    } catch (Exception ee) {
      et = expr.etypecheck(ed, eg, ep, true);
    }
    if (!(et instanceof ASTCoLBasicType))
      throw new TypeError("Line " + lineno + " :" + "PRINT : argument not of CoLBasicType");
    rhs.typecheck(ed, eg, ep);
  }

  public Set<String> fn(Set<String> s) {
    s = expr.fn(s);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = expr.fnLinear(s);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTPrintLn p = new ASTPrintLn(expr, rhs.subst(e), nl);
    p.rhs.setanc(p);
    p.expr.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    expr.subs(x, y);
    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Value v = expr.eval(ed, eg);
    if (nl) System.out.println(v.toStr());
    else System.out.print(v.toStr());
    System.out.flush();
    rhs.runproc(ep, ed, eg, logger);
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    String str = nl ? "println (" : "print (";
    return str + expr.toStr(ep) + ");\n" + rhs.toStr(ep);
  }

  public void show() {
    System.out.println(this);
    rhs.show();
  }

  static ASTType negtype = new ASTCointT();

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    if (!promoted) {
      Set<String> fns = new HashSet<String>();

      fns = expr.fn(fns);
      ASTNode base = new ASTPrintLn(expr, rhs, nl);
      ((ASTPrintLn) base).promoted = true;
      for (String id : fns) {
        SessionField sf = frame.find(id);
        if (sf instanceof SessionClosure) {
          base = new ASTCall(id, "$" + id, negtype, base); // any negative type will do
          //		    System.out.println("SessionClosure added Call "+id);
        }
      }
      base.sam(frame, ep);

    } else {

      Value v = expr.sameval(frame);
      if (nl) System.out.println(v.toStr());
      else System.out.print(v.toStr());
      System.out.flush();
      rhs.sam(frame, ep);
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    if (CLLSj.trace) {
      System.out.println("println-lib " + expr);
    }
    /*
    if (false && !promoted) { // deactivated call insert here! job done in VID
        Set<String> fns = new HashSet<String>();

        fns = expr.fn(fns);
        ASTNode base = new ASTPrintLn(expr,rhs,nl);
        ((ASTPrintLn)base).promoted = true;
        for (String id : fns) {
    	SessionField sf = frame.find(id);
    	if (sf instanceof SessionClosure) {
    	    base = new ASTCall(id,"$"+id,negtype,base);
    	    System.out.println("Println added Call "+id);
    	}
        }
        p_cont.code = base;
        p_cont.frame = frame;
        p_cont.epnm = ep;

        } else
    */
    {
      Value v = expr.sameval(frame);
      if (nl) System.out.println(v.toStr());
      else System.out.print(v.toStr());
      System.out.flush();
      p_cont.code = rhs;
      // p_cont.frame = frame;
      p_cont.epnm = ep;
    }
  }
}
