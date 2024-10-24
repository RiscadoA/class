package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Pair;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.types.ASTAffineT;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTStructT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTSSelect extends ASTNode {
  String ch;
  String lab;
  String cho;
  ASTNode lhs;
  ASTNode rhs;
  ASTType tys_lhs;
  ASTType type;
  ASTType contType;

  public ASTSSelect(String _ch, String _lab, ASTType _t, String _sn, ASTNode _lhs, ASTNode _rhs) {
    ch = _ch;
    lab = _lab;
    cho = _sn;
    type = _t;
    rhs = _rhs;
    lhs = _lhs;
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

  void addAffines(ASTStructT ty, int n) throws Exception {
    Function<ASTNode, ASTNode> f = (ASTNode x) -> new ASTAffine(ch, x);
    anc.ASTInsertPipe(f, this);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    ASTType typee = null;
    ASTType ty = ed.find(ch);
    int affine_count = 0;

    this.eg = eg;
    this.inferUses(ch, ed, ep);

    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, ch, ep);

    if (type != null) {
      typee = (type != null) ? type.unfoldType(ep) : type;
      typee = ASTType.unfoldRec(typee);
    }

    while (ty instanceof ASTAffineT) {
      ty = ((ASTAffineT) ty).getin();
      ty = ty.unfoldType(ep);
      ty = ASTType.unfoldRec(ty);
      affine_count++;
    }

    if (ty instanceof ASTStructT) {
      ASTStructT tys = (ASTStructT) ty;

      System.out.println("SELECT " + ch + "." + lab);

      if (affine_count > 0 && ASTType.affine(tys, ep)) {
        System.out.println("Add affines !");
        addAffines(tys, affine_count);
      }

      // for SAM
      // offset = tys.GetOffset();
      type = tys;
      // System.out.println("SEND offset "+chs+"@"+offset);
      // was for SAM

      Pair<ASTType, ASTType> st = tys.step(lab, (affine_count > 0));

      if (st == null)
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "STRUCT "
                + ch
                + " type mismatch: label "
                + lab
                + " not found in "
                + tys.toStr(ep));
      tys_lhs = st.fst.unfoldType(ep);
      tys_lhs = ASTType.unfoldRec(tys_lhs);

      contType = st.snd.unfoldType(ep);

      // System.out.println("SELECT "+lab+ "tys?"+tys + " cont="+contType.toStr(ep));

      ed.upd(ch, null); // update session channel

      Env<ASTType> el = ed.assoc(cho, tys_lhs);
      if (typee != null && !typee.equalst(tys_lhs, ep, true, new Trail()))
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "STRUCT "
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
                    + ch
                    + ": cannot be parsed as send of basic expression nor as free output.");
        }
        lhs.setanc(this);
      }

      ep = ASTNode.propagateRVar(ep, ch, cho);

      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

      lhs.typecheck(el, eglhs, ep);
      lhs.linclose(el, ep);

      lhs = ASTInferLinClose(lhs, cho, el, ep);

      ed.upd(ch, contType.unfoldType(ep));

      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());

      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "STRUCT: "
              + ch
              + " is not of STRUCT type, found: "
              + ty.toStr(ep));
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    s = lhs.fn(s);
    s.remove(cho);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(ch);
    s = lhs.fnLinear(s);
    s.remove(cho);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTSSelect p = new ASTSSelect(ch, lab, type, cho, lhs.subst(e), rhs.subst(e));
    p.lineno = this.lineno;
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) {
      ch = x;
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
    Channel channel = (Channel) ed.find(ch);
    LinSession sessionOut = new LinSession(cho);

    System.out.println("+SSELECT: " + lab + "@" + ch);
    channel.send(lab, sessionOut);
    System.out.println("-SSELECT: " + lab + "@" + ch);

    // System.out.println("ASTSelect send  on  " + ch + " @ "+lab);

    // System.out.println("SSELECT on session " + ch + " @ "+channel.getId());
    // System.out.println("[RunStatus] SSELECT on "+sessionOut.id+" end.");

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

    rhs.runproc(ep, ed, eg, logger);
    /*
      CLLSj.threadPool.submit(
      new Runnable(){public void run(){
      try{
      rhs.runproc(ep, ed, eg, logger);
      }catch (Exception e){ e.printStackTrace(System.out); }
      }});
    */
  }

  public void show() {
    System.out.println(this);
    rhs.show();
  }
}
