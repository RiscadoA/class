package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.TypeClosure;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTSendTT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTSendTy extends ASTNode {
  String chs;
  ASTType type;
  ASTType type_rhs;
  ASTNode rhs;

  public ASTSendTy(String _chs, ASTType _type, ASTNode _rhs) {
    chs = _chs;
    type = _type;
    rhs = _rhs;
  }

  public void setChs(String chs) {
    this.chs = chs;
  }

  public String getChs() {
    return chs;
  }

  public ASTType getType() {
    return type;
  }

  public ASTType getTypeRhs() {
    return type_rhs;
  }

  public ASTNode getRhs() {
    return rhs;
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
    if (_ch.equals(chs)) {
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

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
    return this;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(chs)) {
      ASTNode pushWhy = new ASTWhy(_ch, _t, here);
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    this.inferUses(chs, ed, ep);

    ASTType ty = ed.find(chs);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);
    type = type.unfoldType(ep);
    if (ty instanceof ASTSendTT) {
      ASTSendTT tys = (ASTSendTT) ty;
      type.kindcheck(ep);

      Env<ASTType> ee = new Env<ASTType>().assoc(tys.getid(), type);

      type_rhs = tys.getrhs().unfoldType(ep).subst(ee);
      ed.upd(chs, type_rhs);
      rhs.typecheck(ed, eg, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "SENDT: " + chs + " is not of SENDT type: " + ty);
  }

  public Set<String> fn(Set<String> s) {
    s.add(chs);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(chs);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTSendTy p = new ASTSendTy(chs, type.subst(e), rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chs) chs = x;

    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(chs);
    ASTType typeSend = type.unfoldType(ep);
    //		System.out.println("SENDING TYPE : " + typeSend.toStr(ep) + " on session " + chs + " "+
    // channel.getId());
    channel.send(typeSend);
    //		System.out.println("SENT TYPE : " + typeSend.toStr(ep) + " on session " + channel.getId());
    logger.info("SENDING TYPE " + typeSend.toStr(ep) + " on session " + channel.getId());
    // System.out.println("[RunStatus] CLOSE on "+session.id+" end.");
    rhs.runproc(ep, ed, eg, logger);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    IndexedSessionRef sref = (IndexedSessionRef) frame.find(chs);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();

    /** work todo here */

    /** do polarity switch right !! */
    if (srec.getPol()) { //  polarity +

      if (CLLSj.trace) {
        System.out.println("sendty-op " + chs + " " + srec + " @ " + doffset);
      }
      /*
        type = type.unfoldType(ep);
        srec.writeSlot( new TypeClosure(type,ep), doffset);
        sref.incOffset();

        ASTNode cont = srec.getCont();
        Env<SessionField> frm  = srec.getFrame();
        Env<EnvEntry> epn = srec.getFrameP();

                    boolean pold = srec.getPolDual();

                    // srec.setPolDual(srec.getPol());
                    // srec.setPol(pold);


                    srec.setCont(rhs);
                    srec.setPol(false);

                    srec.setcch(chs);
                    srec.setFrame(frame);
                    srec.setFrameP(ep);

                    p_cont.code = cont;
                    p_cont.frame = frm;
                    p_cont.epnm = epn;
      */

      type = type.unfoldType(ep);
      srec.writeSlot(new TypeClosure(type, ep), doffset);
      sref.incOffset();

      if (type_rhs.isPos(ep)) {
        // System.out.println("SENDTY POS cont "+type_rhs);
        p_cont.code = rhs;
        p_cont.frame = frame;
        p_cont.epnm = ep;
      } else {

        ASTNode cont = srec.getCont();
        Env<SessionField> frm = srec.getFrame();
        Env<EnvEntry> epn = srec.getFrameP();

        boolean pold = srec.getPolDual();

        // srec.setPolDual(srec.getPol());
        // srec.setPol(pold);

        srec.setCont(rhs);
        srec.setPol(false);

        srec.setcch(chs);
        srec.setFrame(frame);
        srec.setFrameP(ep);

        p_cont.code = cont;
        p_cont.frame = frm;
        p_cont.epnm = epn;
      }

    } else {
      throw new SAMError("sendty-op - " + chs);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
