package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Cell;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.TypeError;

public class ASTUnlock extends ASTNode {
  String chr;
  ASTNode rhs;

  public ASTUnlock(String _chr, ASTNode _rhs) {
    chr = _chr;
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
    anc.ASTInsertUse(_ch, t, this, disCont);
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;
    here.setanc(pushCall);
    pushCall.setanc(this);
    rhs = pushCall;
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    this.inferUses(chr, ed, ep);
    ASTType ty = ed.find(chr);
    ty = ty.unfoldType(ep);
    if (ty instanceof ASTUsageBLT) {
      ASTUsageBLT tyr = (ASTUsageBLT) ty;
      ed.upd(chr, new ASTUsageBT(tyr.getin().unfoldType(ep)));
      rhs.typecheck(ed, eg, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "UNLOCK: " + chr + " is not of LOCKED USAGE! type.");
  }

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public Set<String> fn(Set<String> s) {
    Set<String> ss;
    s.add(chr);
    ss = rhs.fn(s);
    return ss;
  }

  public Set<String> fnLinear(Set<String> s) {
    Set<String> ss;
    s.add(chr);
    ss = rhs.fnLinear(s);
    return ss;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTUnlock p = new ASTUnlock(chr, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chr) chr = x;

    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Cell cell = (Cell) ed.find(chr);
    cell.unlock();
    logger.info("UNLOCK cell " + cell.getId());
    rhs.runproc(ep, ed, eg, logger);
  }
}
