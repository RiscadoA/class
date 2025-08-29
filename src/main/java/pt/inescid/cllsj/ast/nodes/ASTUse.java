package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionFieldAffine;
import pt.inescid.cllsj.SessionFieldUse;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTCoAffineT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTUse extends ASTNode {
  String ch;
  ASTNode rhs;
  ASTType contType;

  public ASTUse(String _ch, ASTNode _rhs) {
    ch = _ch;
    rhs = _rhs;
  }

  public String getCh() {
    return ch;
  }

  public ASTNode getRhs() {
    return rhs;
  }

  public void setCh(String ch) {
    this.ch = ch;
  }

  public void show() {
    System.out.println(this);
    rhs.show();
  }

  public ASTType getContType() {
    return contType;
  }

  public void setrhs(ASTType t) {
    contType = t;
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
      ASTNode pushWhy = new ASTWhy(_ch, _t, here);
      pushWhy.eg = eg;
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    ASTType ty = ed.find(ch);
    ty = ty.unfoldType(ep);
    if (!(ty instanceof ASTCoAffineT)) // exclude
    ty = ASTType.unfoldRecInfer(ty, this, ch, ep);
    if (ty instanceof ASTCoAffineT) {
      ASTCoAffineT tyr = (ASTCoAffineT) ty;
      contType = tyr.getin().unfoldType(ep);
      // System.out.println("USE contType "+contType);
      ed.upd(ch, contType);
      rhs.typecheck(ed, eg, ep);
      rhs.linclose(ed, ep);
    } else
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "USE: "
              + ch
              + " is not of COAFFINE type, found: "
              + ty.toStr(ep));
  }

  public Set<String> fn(Set<String> s) {
    s = rhs.fn(s);
    s.add(ch);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = rhs.fnLinear(s);
    s.add(ch);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTUse p = new ASTUse(ch, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) ch = x;

    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(ch);

    // System.out.println("RUN-USE "+ch);

    CLLSj.inc_coaff(+1);
    channel.send("USE");
    CLLSj.inc_coaff(-1);

    //	System.out.println("- USE "+ch);
    logger.info("USE session " + channel.getId());
    rhs.runproc(ep, ed, eg, logger);
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    return "use " + ch + ";\n" + rhs.toStr(ep);
  }

  static SessionFieldUse SFUSE = new SessionFieldUse();

  public void samCUse(Channel channel, Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont)
      throws Exception {
    if (CLLSj.trace) {
      System.out.println("use-op-lc " + ch + " " + channel);
    }
    // System.out.println("+USE:  "+ch);
    CLLSj.inc_coaff(+1);
    SessionFieldAffine arg = (SessionFieldAffine) channel.receive();
    CLLSj.inc_coaff(-1);
    // System.out.println("-USE:  "+ch);
    // System.out.println("++USE ACK:  "+ch);
    channel.send(SFUSE); // ack
    // System.out.println("--USE ACK:  "+ch);
    ASTType tyrhs = contType;
    p_cont.code = rhs;
    p_cont.frame = frame;
    p_cont.epnm = ep;
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    SessionField sf = frame.find(ch);

    if (sf instanceof LinSessionValue) {
      LinSessionValue lsv = (LinSessionValue) sf;
      Channel channel = lsv.getLin();
      samCUse(channel, frame, ep, p_cont);

    } else {
      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();

      boolean pol = srec.getPol();
      if (pol) {
        throw new SAMError("use-op + " + ch + " " + contType);
      } else {
        if (CLLSj.trace) {
          System.out.println("use-op " + ch + " " + srec + " @ " + doffset);
        }

        SessionFieldAffine arg = (SessionFieldAffine) srec.readSlot(doffset);
        srec.writeSlot(null, doffset);
        sref.incOffset();
        if (arg == null) throw new SAMError("SAM-USE-read-FAILURE");

        ASTType tyrhs = contType;

        if (tyrhs.isPos(ep)) {
          srec.setPol(true);
          srec.setPolDual(false);

          IndexedSessionRef srefd = (IndexedSessionRef) srec.getFrame().find(srec.getcch());

          sref.resetOffset();
          srefd.resetOffset();
          p_cont.code = rhs;
          p_cont.frame = frame;
          p_cont.epnm = ep;
        } else { // switch to other end-point

          ASTNode cont = srec.getCont();
          Env<SessionField> frm = srec.getFrame();
          Env<EnvEntry> epn = srec.getFrameP();
          boolean pold = srec.getPolDual();

          srec.setPolDual(srec.getPol());
          srec.setPol(pold);

          srec.setCont(rhs);
          // System.out.println("CONT USE "+rhs);
          srec.setcch(ch);
          srec.setFrame(frame);
          srec.setFrameP(ep);

          p_cont.code = cont;
          p_cont.frame = frm;
          p_cont.epnm = epn;
        }
      }
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
