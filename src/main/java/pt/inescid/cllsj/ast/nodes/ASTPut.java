package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Cell;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.MVar;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTAffineT;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageLT;
import pt.inescid.cllsj.ast.types.ASTUsageT;

public class ASTPut extends ASTNode {
  String chs;
  String cho;
  ASTType type;
  ASTNode lhs;
  ASTNode rhs;
  ASTType tys_payl;

  public ASTPut(String _chs, String _cho, ASTType _type, ASTNode _lhs, ASTNode _rhs) {
    chs = _chs;
    cho = _cho;
    type = _type;
    lhs = _lhs;
    rhs = _rhs;
  }

  public String getChs() {
    return chs;
  }

  public String getCho() {
    return cho;
  }

  public ASTNode getLhs() {
    return lhs;
  }

  public ASTNode getRhs() {
    return rhs;
  }

  public ASTType getLhsType() {
    return tys_payl;
  }

  public ASTType getRhsType() {
    return type;
  }

  public void setChs(String _chs) {
    chs = _chs;
  }

  public void setCho(String _cho) {
    cho = _cho;
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
    // this.inferUses(chs,ed,ep);

    ASTType ty = ed.find(chs);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, chs, ep);

    if (ty instanceof ASTUsageLT) {
      ASTUsageLT tys = (ASTUsageLT) ty;
      tys_payl = tys.getin().dual(ep).unfoldType(ep);

      if (type != null) {
        type = type.unfoldType(ep);
        // type = ASTType.unfoldRec(type);
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
            try { // check if the free put is a linear or unrestricted name
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
    ASTPut p;
    if (type == null) p = new ASTPut(chs, cho, type, lhs.subst(e), rhs.subst(e));
    else p = new ASTPut(chs, cho, type.subst(e), lhs.subst(e), rhs.subst(e));
    p.rhs.setanc(p);
    p.lhs.setanc(p);
    return p;
  }

  public void show() {
    System.out.println(this + " (lhs) " + anc);
    lhs.show();
    System.out.println(this + " " + anc);
    rhs.show();
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

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = (SessionField) frame.find(chs);
    int sessionSize = tys_payl.SetOffsets(0, ep) + 2; // to consider affine(tys_payl)
    // System.out.println("sf="+sf+" "+sessionSize+" "+tys_payl);

    if (sf instanceof MVar) {
      MVar cell = (MVar) sf;
      SessionClosure clos =
          new SessionClosure(cho, sessionSize, tys_payl.isPos(ep), lhs, frame, ep);

      if (CLLSj.trace) {
        System.out.println("put-op " + chs + " " + clos);
      }

      cell.put(clos);
      p_cont.code = rhs;
      return;
    } else throw new SAMError("put-op - " + chs);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
