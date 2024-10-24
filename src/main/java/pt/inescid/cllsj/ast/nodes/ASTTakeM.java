package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Cell;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.types.ASTCoMutT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageT;

public class ASTTakeM extends ASTNode {
  String chr;
  String label;
  String chi;
  ASTType type;
  ASTNode rhs;

  public ASTTakeM(String _chr, String _lab, String _chi, ASTType _type, ASTNode _rhs) {
    chr = _chr;
    label = _lab;
    chi = _chi;
    type = _type;
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
    if (_ch.equals(chi)) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.eg = eg;
      pushUse.setrhs(t);
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

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    if (_ch.equals(chi)) {
      if (!_ch.equals(chr)) {
        if (exp) {
          return this.ASTInsertWhy(_ch);
        } else {
          return this.ASTInsertMixDiscard(_ch);
        }
      }
      throw new TypeError(
          "Line " + lineno + " :" + "for " + _ch + " type pending after TAKE on " + chr);
    }
    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
    return this;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    this.inferUses(chr, ed, ep);

    ASTType typee = null;
    ASTType ty = ed.find(chr);
    ty = ty.unfoldType(ep);

    ty = ASTType.unfoldRec(ty);
    if (type != null) {
      typee = type.unfoldType(ep);
      typee = ASTType.unfoldRec(typee);
    }

    if (ty instanceof ASTCoMutT) {
      ASTCoMutT tyr = (ASTCoMutT) ty;

      if (tyr.locked())
        throw new TypeError("Line " + lineno + " :" + "TAKE #L: COMUT in use, must PUT #L.");

      ASTType tylab = tyr.gettype(label).unfoldType(ep);
      ASTType ty_lhs = new ASTUsageT(tylab);

      if (typee != null && !typee.equalst(ty_lhs, ep, true, new Trail()))
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "TAKE "
                + chi
                + " type mismatch: found="
                + ty_lhs.toStr(ep)
                + " declared="
                + typee.toStr(ep));
      Env<ASTType> ext = ed.assoc(chi, ty_lhs);

      ASTCoMutT stepty = tyr.locki(label);

      ext.upd(chr, stepty);
      ep = ASTNode.propagateRVar(ep, chr, chi);

      //  System.out.println("HERE -TAKE");
      rhs.typecheck(ext, eg, ep);
      // System.out.println("HERE +TAKE");

      rhs.linclose(ed, ep);

      // System.out.println("LINC +TAKE");
      // rhs.linclose(chi,ed,ep);

      rhs = ASTInferLinClose(rhs, chi, ext, ep);
    } else
      throw new TypeError("Line " + lineno + " :" + "TAKE #L: " + chr + " is not of COMUT type.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(chr);
    s = rhs.fn(s);
    s.remove(chi);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(chr);
    s = rhs.fnLinear(s);
    s.remove(chi);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTType ts = (type == null) ? type : type.subst(e);
    ASTTakeM p = new ASTTakeM(chr, label, chi, ts, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chr) {
      chr = x;
      rhs.subs(x, y);
    } else if (x == chi) { // we rename the bound name chi to fresh to avoid capturing name x
      String fresh = ASTNode.gensym();
      rhs.subs(fresh, chi);
      chi = fresh;
      rhs.subs(x, y);
    } else if (y != chi) rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Cell cell = (Cell) ed.find(chr);
    LinSession session = cell.take(chi);
    logger.info("TAKE cell " + cell.getId() + " on session " + chi);
    rhs.runproc(ep, ed.assoc(chi, session), eg, logger);
  }
}
