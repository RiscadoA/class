package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.RVarEntry;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionFieldClose;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTAffineT;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTBasicType;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCoAffineT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageT;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public abstract class ASTNode {

  public boolean info = false;

  public int repeat = 1;

  ASTNode anc; // ancestor reference, fdor AST rewriting

  Env<ASTType> eg;

  public int lineno = -1; // line number of node

  static final SessionField CloseTok = new SessionFieldClose();

  public void setanc(ASTNode a) {
    // System.out.println("SET = "+a);
    anc = a;
  }

  public ASTNode getanc() {
    return anc;
  }

  public void show() {
    System.out.println(this);
  }

  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }

  public abstract void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep)
      throws Exception;

  public abstract Set<String> fn(Set<String> e);

  public abstract Set<String> fnLinear(Set<String> e);

  public abstract ASTNode subst(Env<ASTType> e);

  public abstract void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont)
      throws Exception;

  public abstract void ASTInsertWhyNot(String ch, ASTType t, ASTNode here) throws Exception;

  public abstract void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here)
      throws Exception;

  public abstract ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception;

  public abstract void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception;

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new TypeError("ASTInsertPipe not implemented");
  }

  private static int n = 0;

  static String gensym() {
    return "$" + (n++);
  }

  static synchronized void reinit() {
    n = 0;
  }

  public abstract void subs(String x, String y); // Substitutes name x by y in this ASTNode

  public void linclose(Env<ASTType> ed, Env<EnvEntry> ep) throws Exception {
    Set<String> s1 = fn(new HashSet<String>());
    Iterator<String> it = s1.iterator();
    while (it.hasNext()) {
      String id = it.next();
      boolean def = false;
      try {
        def = ed.def(id);
      } catch (Exception e) {
      }
      if (def) {
        ASTType te = ed.find(id);
        if (!(te instanceof ASTCointT)) // NON-LIN-INT
        throw new TypeError(
              "Line " + lineno + " :" + "for " + id + " type pending = " + te.toStr(ep));
      }
    }
    ;
  }

  public void linclose(String id, Env<ASTType> ed, Env<EnvEntry> ep) throws Exception {
    boolean def = false;
    try {
      def = ed.def(id);
    } catch (Exception e) {
    }

    if (def) {
      ASTType te = ed.find(id);
      // System.out.println("lin-close " + id + " " + te);
      if (!(te instanceof ASTCointT)) // NON-LIN-INT
      throw new TypeError(
            "Line " + lineno + " :" + "for " + id + " type pending = " + te.toStr(ep));
    }
  }

  /*
    Returns a new session directory obtained by restricing session_dir to set of free names of this
  */
  public HashMap<String, Session> resToFn(HashMap<String, Session> session_dir) {
    HashMap<String, Session> session_dir_out = new HashMap<String, Session>();
    Iterator<String> fn_it = fn(new HashSet<String>()).iterator();
    while (fn_it.hasNext()) {
      String ch = fn_it.next();
      if (session_dir.containsKey(ch)) {
        session_dir_out.put(ch, session_dir.get(ch));
      }
    }
    return session_dir_out;
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    throw new RuntimeException("Unreachable");
  }

  public static Env<EnvEntry> propagateRVar(Env<EnvEntry> ep, String parent, String child) {
    try {
      RVarEntry r = (RVarEntry) ep.find(parent);
      return ep.assoc(child, new RVarEntry());
    } catch (Exception e) {
      return ep;
    }
  }

  public static ASTNode ForMacroFactory(
      String c1, ASTType t, String c2, String c3, String c4, String c5, ASTNode body) {

    String step = ASTType.gensym();
    String loop = step + "loop";
    ASTNode ast = new ASTSend(step, c2, null, body, new ASTClose(step));
    ast = new ASTRecv(step, c1, null, ast);
    ast = new ASTBang(loop, step, null, ast);
    ASTId iter = new ASTId("iters");
    iter.addTpar(t);
    ASTVId c4VId = new ASTVId(c4);
    iter.addExpr(c4VId);
    ASTVId c3VId = new ASTVId(c3);
    iter.addExpr(c3VId);
    ASTVId loopVId = new ASTVId(loop);
    iter.addGExpr(loopVId);
    ASTVId c5VId = new ASTVId(c5);
    iter.addGExpr(c5VId);
    ASTNode ast2 = new ASTWhy(loop, iter);
    ASTType tc = new ASTWhyT(new ASTSendT(t, new ASTRecvT(new ASTNotT(t), new ASTBotT())));
    ast = new ASTCut(loop, tc, ast, ast2);
    return ast;
  }

  public void inferUses(String chs, Env<ASTType> ed, Env<EnvEntry> ep) throws Exception {
    ASTType ty = ed.find(chs);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);
    if (ty instanceof ASTCoAffineT) {
      ASTCoAffineT tyco = (ASTCoAffineT) ty;
      ed.upd(chs, tyco.getin());
      ASTType cont = tyco.getin().unfoldType(ep);
      cont = ASTType.unfoldRec(cont);
      Boolean disposableCont =
          (cont instanceof ASTUsageT)
              || (cont instanceof ASTCoAffineT)
              || (cont instanceof ASTWhyT);
      this.getanc().ASTInsertUse(chs, tyco.getin(), this, disposableCont);
      this.inferUses(chs, ed, ep);
    }
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    throw new TypeError("toStr not implemented");
  }

  public ASTNode ASTInsertWhy(String _ch) {
    ASTNode push = new ASTWhy(_ch, this);
    this.setanc(push);
    push.setanc(anc);
    return push;
  }

  public ASTNode ASTInsertMixDiscard(String _ch) {
    // System.out.println("ASTInsertMixDiscard "+this);
    ASTNode push = new ASTDiscard(_ch);
    ASTNode mix = new ASTMix(false, push, this);
    mix.setanc(anc);
    this.setanc(mix);
    push.setanc(mix);
    return mix;
  }

  public ASTNode ASTweakeningHere(String _ch, ASTType t, boolean exp) {
    // System.out.println("ASTweakeningHere ");
    if (exp) {
      ASTNode push = new ASTWhy(_ch, t, this);
      this.setanc(push);
      push.setanc(anc);
      return push;
    } else {
      return this.ASTInsertMixDiscard(_ch);
    }
  }

  public ASTNode ASTweakeningTerm(String _ch, boolean exp) throws Exception {
    if (exp) {
      // System.out.println("ASTInsertWhy "+exp);
      return this.ASTInsertWhy(_ch);
    } else {
      // System.out.println("ASTInsertMixDiscard "+exp);
      return this.ASTInsertMixDiscard(_ch);
    }
  }

  /* this called at the end of scope of _ch */
  public ASTNode ASTInferLinClose(ASTNode node, String _ch, Env<ASTType> ed, Env<EnvEntry> ep)
      throws Exception {

    try {
      node.linclose(_ch, ed, ep);
      return node;
    } catch (Exception e) {
      ASTType gen = ed.find(_ch);
      boolean exp = gen instanceof ASTWhyT;
      if (exp || gen instanceof ASTCoAffineT) {
        // System.out.println("Weakening inferred: "+weakop(exp) + _ch);
        return node.ASTweakeningOnLeaf(_ch, gen, exp);
        // lhs.show();;
      } else throw e;
    }
  }

  static String weakop(boolean exp) {
    if (exp) return "?";
    else return "discard ";
  }

  public boolean isPos() {
    ASTNode it = this;
    return (it instanceof ASTClose
        || it instanceof ASTSend
        || it instanceof ASTPrintLn
        || it instanceof ASTCut
        || it instanceof ASTFwd
        || it instanceof ASTSelect
        || it instanceof ASTCall
        || it instanceof ASTSendTy
        || it instanceof ASTEmpty
        || it instanceof ASTBang
        || (it instanceof ASTUnfold && ((ASTUnfold) it).rec));
  }

  public String getSubjectCh() {
    return null;
  }

  public ASTNode compileFwd(String x1, String x2, ASTType t1, ASTType t2, Env<EnvEntry> ep)
      throws Exception {
    // TODO: deal wiht bang and use this for send and cell, besides put
    t1 = t1.unfoldType(ep);
    t1 = ASTType.unfoldRec(t1);

    t2 = t2.unfoldType(ep);
    t2 = ASTType.unfoldRec(t2);

    ASTType t2Dual = t2.dual(ep);

    if (t1.equalst(t2Dual, ep, true, new Trail())) {
      return new ASTFwd(x1, x2);
    } else if (t1 instanceof ASTCoAffineT) {
      ASTCoAffineT tyco = (ASTCoAffineT) t1;
      t1 = tyco.getin();
      return compileFwd(x1, x2, t1, t2, ep);
    } else if (t2 instanceof ASTCoAffineT) {
      ASTCoAffineT tyco = (ASTCoAffineT) t1;
      t2 = tyco.getin();
      return compileFwd(x1, x2, t1, t2, ep);
    } else if (t1 instanceof ASTAffineT) {
      t1 = ((ASTAffineT) t1).getin();
      ASTNode p = compileFwd(x1, x2, t1, t2, ep);
      ASTNode q = new ASTAffine(x1, p);
      p.setanc(q);
      return q;
    } else throw new TypeError("Line " + lineno + " :" + "Compilation of forwarder failed.");
  }

  public ASTNode compileExpr(String cho, ASTExpr pe, ASTType t, Env<EnvEntry> ep) throws Exception {
    if (t instanceof ASTBasicType) {
      ASTNode lhs = new ASTCoExpr(cho, pe);
      pe.setanc(lhs);
      return lhs;
    } else if (t instanceof ASTBangT) {
      ASTType tpayl = ((ASTBangT) t).getin();
      tpayl = tpayl.unfoldType(ep);
      tpayl = ASTType.unfoldRec(tpayl);
      if (tpayl instanceof ASTBasicType) {
        ASTNode lhs = new ASTPromoCoExpr(cho, pe);
        pe.setanc(lhs);
        return lhs;
      } else {
        String chi = ASTType.gensym();
        ASTNode lhsc = compileExpr(chi, pe, tpayl, ep);
        ASTNode lhs = new ASTBang(cho, chi, tpayl, lhsc);
        lhsc.setanc(lhs);
        return lhs;
      }
    } else if (t instanceof ASTAffineT) {
      ASTType tpayl = ((ASTAffineT) t).getin();
      tpayl = tpayl.unfoldType(ep);
      tpayl = ASTType.unfoldRec(tpayl);
      ASTNode lhsc = compileExpr(cho, pe, tpayl, ep);
      ASTNode lhs = new ASTAffine(cho, lhsc);
      lhsc.setanc(lhs);
      return lhs;
    } else
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "Compilation of expression failed. "
              + "Type "
              + t.toStr(ep)
              + " is not of the form (affine + !)^*BasicType.");
  }

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    System.out.println(this + ":SAM-Not-Implemented");
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont cont) throws Exception {
    System.out.println(this + ":SAML-Not-Implemented");
  }
}
