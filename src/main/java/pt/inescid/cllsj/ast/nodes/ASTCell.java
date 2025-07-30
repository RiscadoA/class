package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Cell;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.MVar;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTAffineT;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCellT;
import pt.inescid.cllsj.ast.types.ASTType;

// unfolded

public class ASTCell extends ASTNode {
  String ch;
  String chc;
  ASTType type;
  ASTNode rhs;
  boolean linear;
  boolean statecell;
  ASTType ty_rhs;

  public ASTCell(String _chr, String _chc, ASTType _type, ASTNode _rhs) {
    ch = _chr;
    chc = _chc;
    type = _type;
    rhs = _rhs;
  }

  public String getCh() {
    return ch;
  }

  public String getChc() {
    return chc;
  }

  public ASTType getTypeRhs() {
    return ty_rhs;
  }

  public ASTNode getRhs() {
    return rhs;
  }

  public void setCh(String ch) {
    this.ch = ch;
  }

  public void setChc(String chc) {
    this.chc = chc;
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
      ASTNode pushWhy = new ASTWhy(_ch, _t, here);
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
        return this.ASTweakeningHere(_ch, typ, exp);
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

  static ASTType rewpaytype(ASTType payl) {
    if (payl instanceof ASTCellT || payl instanceof ASTAffineT) {
      return payl;
    } else {
      return new ASTAffineT(payl);
    }
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    // this.inferUses(ch,ed,ep);

    ASTType typeex = null;
    ASTType ty = ed.find(ch);
    if (type != null) {
      typeex = type.unfoldType(ep); // bug here because this will not match unfoldRecInfer
      typeex = ASTType.unfoldRec(typeex);
    }
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, ch, ep);

    if (ty instanceof ASTCellT) {
      ASTCellT tyr = (ASTCellT) ty;
      ASTType payl2 = tyr.getin().unfoldType(ep);

      ASTType payl = ASTCell.rewpaytype(payl2);
      ty_rhs = payl;

      statecell = (payl instanceof ASTCellT); // for the interpreter

      ed.upd(ch, null);

      if (typeex != null && !typeex.equalst(payl, ep, true, new Trail())) {
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "CELL "
                + chc
                + " type mismatch; found="
                + payl.toStr(ep)
                + " expected="
                + typeex.toStr(ep));
      }

      ed = ed.assoc(chc, (payl));

      if (rhs instanceof ASTExpr) {
        ASTExpr pexpr = (ASTExpr) rhs;
        try {
          ASTNode rhsc = compileExpr(chc, pexpr, payl, ep);
          rhs = rhsc;
          rhs.setanc(this);
        } catch (Exception ee) {
          if (pexpr instanceof ASTVId) {
            String x = ((ASTVId) pexpr).ch;
            try { // checks if free name is linear or unrestricted
              ed.find(x);
              rhs = new ASTFwd(chc, x);
            } catch (Exception e) {
              ASTFwdB f = new ASTFwdB(chc, x);
              rhs = new ASTAffine(chc, f);
              f.setanc(rhs);
            }
          } else
            throw new TypeError(
                "Line "
                    + lineno
                    + " :"
                    + "CELL "
                    + ch
                    + ": cannot be parsed as cell of basic expression nor as free cell.");
        }
      }

      rhs.setanc(this);

      Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

      // lhs = ASTInferLinClose(lhs,cho,ed,ep);

      rhs.typecheck(ed, eglhs, ep);
      rhs.linclose(ed, ep);
      rhs.linclose(chc, ed, ep);
      linear = true;

    } else
      throw new TypeError(
          "Line " + lineno + " :" + "CELL: " + ch + " is neither of STATE nor of STATE! type.");
  }

  public Set<String> fn(Set<String> s) {
    s = rhs.fn(s);
    s.remove(chc);
    s.add(ch);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = rhs.fnLinear(s);
    s.remove(chc);
    s.add(ch);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTType ts = type == null ? null : type.subst(e);
    ASTCell p = new ASTCell(ch, chc, ts, rhs.subst(e));
    p.rhs.setanc(p);
    p.linear = linear;
    p.statecell = statecell;
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x) in place
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
    cell.setCell(chc, rhs, ep, ed, eg, logger, linear, statecell);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = frame.find(ch);
    int sessionSize = ty_rhs.SetOffsets(0, ep) + 1;

    if (sf instanceof LinSessionValue) {
      int i = 2 / 0;
    } else {

      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();

      SessionClosure clos = new SessionClosure(chc, sessionSize, ty_rhs.isPos(ep), rhs, frame, ep);
      MVar v = MVar.newMVar();
      v.set(clos);
      if (srec.getPol()) {
        srec.writeSlot(v, doffset);
        sref.incOffset();

        ASTNode cont = srec.getCont();
        Env<SessionField> frm = srec.getFrame();
        Env<EnvEntry> epn = srec.getFrameP();
        boolean pold = srec.getPolDual();

        srec.setPolDual(srec.getPol());
        srec.setPol(pold);

        srec.setCont(null);
        srec.setcch(ch);
        srec.setFrame(frame);
        srec.setFrameP(ep);
        p_cont.code = cont;
        p_cont.frame = frm;
        p_cont.epnm = epn;

        if (CLLSj.trace) {
          System.out.println("cell-op " + ch + " " + srec + " @ " + doffset + " " + v);
        }

      } else {
        throw new SAMError("cell-op - " + ch);
      }
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
