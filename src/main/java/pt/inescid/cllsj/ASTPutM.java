package pt.inescid.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTPutM extends ASTNode {
  String chs;
  String label;
  String cho;
  ASTType type;
  ASTNode lhs;
  ASTNode rhs;

  public ASTPutM(String _chs, String _lab, String _cho, ASTType _type, ASTNode _lhs, ASTNode _rhs) {
    chs = _chs;
    cho = _cho;
    label = _lab;
    type = _type;
    lhs = _lhs;
    rhs = _rhs;
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
    anc.ASTInsertUse(_ch, t, this, disCont);
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;

    here.setanc(pushCall);
    pushCall.setanc(this);
    if (lhs == here) lhs = pushCall;
    else rhs = pushCall;
  }

  /* this is BUGGY, because it ignores comming from  lhs and rhs */

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType t, boolean exp) throws Exception {
    Set<String> s = lhs.fn(new HashSet<String>());
    if (!ch.equals(cho) && s.contains(ch)) {
      lhs = lhs.ASTweakeningOnLeaf(ch, t, exp);
      return this;
    }
    rhs = rhs.ASTweakeningOnLeaf(ch, t, exp);
    return this;
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == lhs) lhs = newCont;
    else rhs = newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;
    this.inferUses(chs, ed, ep);

    ASTType ty = ed.find(chs);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);

    if (ty instanceof ASTUsageLT) {
      ASTUsageLT tys = (ASTUsageLT) ty;
      ASTType tys_payl = ASTType.unfoldRec(tys.getin().dual(ep).unfoldType(ep));

      if (type != null) {
        type = type.unfoldType(ep);
        type = ASTType.unfoldRec(type);
        if (!type.equalst(new ASTAffineT(tys_payl), ep, true, new Trail()))
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "PUT "
                  + cho
                  + " type mismatch: found="
                  + tys.toStr(ep)
                  + " declared="
                  + type.toStr(ep));
      }

      ed.upd(chs, null);
      ed = ed.assoc(cho, new ASTAffineT(tys_payl));

      if (lhs instanceof ASTExpr) {
        ASTExpr pe = (ASTExpr) lhs;
        try {
          ASTNode lhsc = compileExpr(cho, pe, tys_payl, ep);
          lhs = new ASTAffine(cho, lhsc);
          lhsc.setanc(lhs);
        } catch (Exception ee) {
          if (pe instanceof ASTVId) {
            String x = ((ASTVId) pe).ch;
            try { // check if the free put is of a linear or unrestricted name
              ASTType t2 = ed.find(x);
              lhs = compileFwd(cho, x, new ASTAffineT(tys_payl), t2, ep);
              lhs.setanc(this);
            } catch (Exception e) {
              ASTFwdB f = new ASTFwdB(cho, x);
              lhs = new ASTAffine(cho, f);
              f.setanc(lhs);
              lhs.setanc(this);
            }
          } else
            throw new TypeError(
                "Line "
                    + lineno
                    + " :"
                    + "PUT "
                    + chs
                    + ": cannot be parsed as put of basic expression nor as free put.");
        }
        lhs.setanc(this);
      }

      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

      lhs.typecheck(ed, eglhs, ep);
      lhs.linclose(ed, ep);
      lhs.linclose(cho, ed, ep);

      ed.upd(chs, new ASTUsageT(tys.getin().unfoldType(ep)));

      Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());

      rhs.typecheck(ed, egrhs, ep);
      rhs.linclose(ed, ep);
    } else throw new TypeError("Line " + lineno + " :" + "PUT: " + chs + " is not of USAGEL type.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(chs);
    s = lhs.fn(s);
    s.remove(cho);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(chs);
    s = lhs.fnLinear(s);
    s.remove(cho);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTPutM p;
    if (type == null) p = new ASTPutM(chs, label, cho, type, lhs.subst(e), rhs.subst(e));
    else p = new ASTPutM(chs, label, cho, type.subst(e), lhs.subst(e), rhs.subst(e));
    p.rhs.setanc(p);
    p.lhs.setanc(p);
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
    Cell cell = (Cell) ed.find(chs);
    cell.put(cho, lhs, ep, ed, eg);
    logger.info("PUT on cell " + cell.getId());
    rhs.runproc(ep, ed, eg, logger);
  }
}
