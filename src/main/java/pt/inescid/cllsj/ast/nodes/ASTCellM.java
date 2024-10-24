package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Cell;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Pair;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SyntaxError;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;

// unfolded

public class ASTCellM extends ASTNode {
  String ch;
  HashMap<String, Pair<String, ASTType>> slots;
  String chc;
  ASTType type;
  boolean linear;
  ASTNode rhs;

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

  public void addSlot(String label, String id, ASTType t) throws Exception {
    if (slots.putIfAbsent(label, new Pair<String, ASTType>(id, t)) != null)
      throw new SyntaxError("Duplicate Label " + label + " in mut struct.");
  }

  public void setrhs(ASTNode r) {
    rhs = r;
  }

  public ASTNode getrhs() {
    return rhs;
  }

  public ASTCellM(String _chr) {
    ch = _chr;
    slots = new HashMap<String, Pair<String, ASTType>>();
  }

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    if (_ch.equals(chc)) {
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
    if (_ch.equals(chc)) {
      ASTNode pushWhy = new ASTWhy(_ch, here);
      pushWhy.eg = eg;
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    if (_ch.equals(chc)) { // may place before cell
      if (!_ch.equals(ch)) {
        return this.ASTweakeningHere(_ch, exp);
      }
      throw new TypeError(
          "Line " + lineno + " :" + "for " + _ch + " type pending in CELL on " + chc);
    }
    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
    return this;
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    this.eg = eg;
    this.inferUses(ch, ed, ep);

    ASTType ty = ed.find(ch);
    HashSet<String> dl = new HashSet<String>();

    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);

    if (ty instanceof ASTMutT) {
      ASTMutT tyr = (ASTMutT) ty;

      ed.upd(ch, null);

      for (Iterator<String> is = slots.keySet().iterator(); is.hasNext(); ) {
        String lab = is.next();
        Pair<String, ASTType> pp = slots.get(lab);
        String idB = pp.fst;
        ASTType tyB = pp.snd;
        ASTType labeltype = ((ASTMutT) ty).gettype(lab);

        if (tyB != null && !tyB.equalst(labeltype, ep, true, new Trail())) {
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "mut "
                  + lab
                  + " type mismatch; found="
                  + labeltype.toStr(ep)
                  + " expected="
                  + tyB.toStr(ep));
        }
        dl.add(idB);
        ed = ed.assoc(idB, new ASTCellT(labeltype));
      }
      rhs.setanc(this);
      rhs.typecheck(ed, eg, ep);
      for (Iterator<String> is = dl.iterator(); is.hasNext(); ) {
        rhs.linclose(is.next(), ed, ep);
      }
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "CELL: " + ch + " is neither of STATE nor of STATE! type.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    s = rhs.fn(s);
    s.remove(chc);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(ch);
    s = rhs.fnLinear(s);
    s.remove(chc);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTType ts = type == null ? null : type.subst(e);
    ASTCellM p = new ASTCellM(ch);
    p.slots = slots;
    p.rhs = rhs;
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) ch = x;
    else if (x == chc) { // we rename the bound name chs to fresh to avoid capturing name x
      String fresh = ASTNode.gensym();
      rhs.subs(fresh, chc);
      chc = fresh;
      rhs.subs(x, y);
    } else if (y != chc) rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Cell cell = (Cell) ed.find(ch);
    cell.setCell(chc, rhs, ep, ed, eg, logger, linear);
  }
}
