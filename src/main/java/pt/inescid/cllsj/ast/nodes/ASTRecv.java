package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTRecv extends ASTNode {
  String chr;
  String chi;
  ASTType type;
  ASTType typesam;
  ASTType tyrhs;
  ASTNode rhs;

  public ASTRecv(String _chr, String _chi, ASTType _type, ASTNode _rhs) {
    chr = _chr;
    chi = _chi;
    type = _type;
    rhs = _rhs;
  }

  @Override
  public String getSubjectCh() {
    return chr;
  }

  public String getChr() {
    return chr;
  }

  public String setChr(String ch) {
    return chr = ch;
  }

  public String getChi() {
    return chi;
  }

  public String setChi(String ch) {
    return chi = ch;
  }

  public ASTType getChiType() {
    return type;
  }

  public ASTNode getRhs() {
    return rhs;
  }

  public ASTType getRhsType() {
    return tyrhs;
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
    if (_ch.equals(chr) || _ch.equals(chi)) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.setrhs(t);
      pushUse.eg = eg;
      here.setanc(pushUse);
      pushUse.setanc(this);
      rhs = pushUse;
    } else anc.ASTInsertUse(_ch, t, this, disCont);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;
    here.setanc(pushCall);
    pushCall.setanc(this);
    rhs = pushCall;
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    if (_ch.equals(chi)) {
      if (!_ch.equals(chr)) { // may place here before receeive
        return this.ASTweakeningHere(_ch, t, exp);
      }
      throw new TypeError(
          "Line " + lineno + " :" + "for " + _ch + " type pending after RECV on " + chr);
    }
    rhs = rhs.ASTweakeningOnLeaf(_ch, t, exp);
    return this;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(chr) || _ch.equals(chi)) {
      ASTNode pushWhy = new ASTWhy(_ch, _t, here);
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    this.eg = eg;
    // this.inferUses(chr,ed,ep);

    ASTType typee = null;
    ASTType ty = ed.find(chr);
    if (type != null) {
      typee = type.unfoldType(ep);
      // typee = ASTType.unfoldRec(typee);
    }
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, chr, ep);
    if (ty instanceof ASTRecvT) {
      ASTRecvT tyr = (ASTRecvT) ty;
      ASTType tyin = tyr.getlhs().unfoldType(ep);
      // tyr = ASTType.unfoldRec(ty);
      type = tyin;

      if (typee != null && !typee.equalst(tyin, ep, true, new Trail()))
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "RECV  "
                + chi
                + " type mismatch; found="
                + tyin.toStr(ep)
                + " declared="
                + typee.toStr(ep));

      tyrhs = tyr.getrhs().unfoldType(ep);
      ed.upd(chr, tyrhs);
      Env<ASTType> ext = ed.assoc(chi, tyin);
      ep = ASTNode.propagateRVar(ep, chr, chi);
      rhs.typecheck(ext, eg, ep);
      rhs.linclose(ed, ep);

      rhs = ASTInferLinClose(rhs, chi, ext, ep);

    } else throw new TypeError("Line " + lineno + " :" + "RECV " + chr + " is not of RECV type.");
  }

  public Set<String> fn(Set<String> s) {
    s = rhs.fn(s);
    s.remove(chi);
    s.add(chr);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = rhs.fnLinear(s);
    s.remove(chi);
    s.add(chr);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTRecv p;
    if (type == null) {
      p = new ASTRecv(chr, chi, type, rhs.subst(e));
      p.typesam = typesam;
    } else {
      p = new ASTRecv(chr, chi, type.subst(e), rhs.subst(e));
      p.typesam = typesam;
    }

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

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(chr);
    // System.out.println("[RunStatus] RECV on "+session.id+" start.");

    // CLLSj.inc_recvs(+1);
    LinSession session_in = (LinSession) channel.receive();
    // CLLSj.inc_recvs(-1);
    // System.out.println("RECV on "+session_in.id+":"+session_in);

    if (type instanceof ASTCointT) {
      // read int and promote id->v to exponential environment
      // no !: use linear environment to store id -> v
      Value v = (Value) session_in.receive();
      ed = ed.assoc(chi, v);
    } else {
      ed = ed.assoc(chi, session_in);
    }

    // System.out.println("[RunStatus] RECV on "+session.id+" end.");
    rhs.runproc(ep, ed, eg, logger);
  }

  public void samLRecv(
      SessionClosure arg, Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont)
      throws Exception {

    String id = arg.getId();
    Env<SessionField> frameloc = arg.getEnv();
    Env<EnvEntry> framep = arg.getEnvP();

    SessionRecord sreco = SessionRecord.newSessionRecord(arg.getSize());
    IndexedSessionRef srecfw = new IndexedSessionRef(0, sreco);
    IndexedSessionRef srecfr = new IndexedSessionRef(0, sreco);

    if (!type.isPos(ep)) { // recv arg is writer U;T U negative

      // System.out.println(" recv arg is writer U;T U negative");

      Env<SessionField> fwrite = frameloc.assoc(id, srecfw);
      Env<SessionField> fread = frame.assoc(chi, srecfr);

      sreco.setPol(true);
      sreco.setPolDual(false);

      sreco.setcch(chi);
      sreco.setCont(rhs);
      sreco.setFrame(fread);
      sreco.setFrameP(ep);

      p_cont.code = arg.getBody();
      p_cont.frame = fwrite;
      p_cont.epnm = framep;

    } else { // recv arg is reader, U;T U positive

      Env<SessionField> fwrite = frame.assoc(chi, srecfw);
      Env<SessionField> fread = frameloc.assoc(id, srecfr);

      sreco.setPol(true);
      sreco.setPolDual(false);

      sreco.setcch(id);
      sreco.setCont(arg.getBody());
      sreco.setFrame(fread);
      sreco.setFrameP(framep);

      p_cont.code = rhs;
      p_cont.frame = fwrite;
      p_cont.epnm = ep;
    }
  }

  public void samLC(Channel channel, Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont)
      throws Exception {
    if (CLLSj.trace) {
      System.out.println("recv-op " + chr + " " + channel);
    }
    SessionClosure arg = (SessionClosure) channel.receive();
    samLRecv(arg, frame, ep, p_cont);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = frame.find(chr);

    if (sf instanceof LinSessionValue) {
      LinSessionValue lsv = (LinSessionValue) sf;
      Channel channel = lsv.getLin();
      samLC(channel, frame, ep, p_cont);

    } else {

      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      boolean pol = srec.getPol();

      if (pol) throw new SAMError("recv-op + " + chr);

      if (CLLSj.trace) {
        System.out.println("recv-op " + chr + " " + srec + " @ " + doffset);
      }

      SessionClosure arg = (SessionClosure) srec.readSlot(doffset);
      if (arg == null) throw new SAMError("SAM-RECV-read-FAILURE");
      String id = arg.getId();

      Env<SessionField> frameloc = arg.getEnv();
      Env<EnvEntry> framep = arg.getEnvP();

      srec.writeSlot(null, doffset); // reset linear value
      sref.incOffset();

      srec.setPol(tyrhs.isPos(ep)); // set polarity endpoint for continuation!!!
      SessionRecord sreco = SessionRecord.newSessionRecord(arg.getSize());

      IndexedSessionRef srecfw = new IndexedSessionRef(0, sreco);
      IndexedSessionRef srecfr = new IndexedSessionRef(0, sreco);

      if (CLLSj.trace) {
        System.out.println("recv-op new = " + sreco);
      }

      if (!type.isPos(ep)) { // recv arg is writer U;T U negative

        // System.out.println(" recv arg is writer U;T U negative");

        Env<SessionField> fwrite = frameloc.assoc(id, srecfw);
        Env<SessionField> fread = frame.assoc(chi, srecfr);

        sreco.setPol(true);
        sreco.setPolDual(false);

        sreco.setcch(chi);
        sreco.setCont(rhs);
        sreco.setFrame(fread);
        sreco.setFrameP(ep);

        if (tyrhs.isPos(ep)) {
          srec.setPol(true);
          srec.setPolDual(false);
          IndexedSessionRef srefd = (IndexedSessionRef) srec.getFrame().find(srec.getcch());
          sref.resetOffset();
          srefd.resetOffset();
        }

        p_cont.code = arg.getBody();
        p_cont.frame = fwrite;
        p_cont.epnm = framep;

        return;

      } else { // recv arg is reader, U;T U positive

        Env<SessionField> fwrite = frame.assoc(chi, srecfw);
        Env<SessionField> fread = frameloc.assoc(id, srecfr);

        sreco.setPol(true);
        sreco.setPolDual(false);

        sreco.setcch(id);
        sreco.setCont(arg.getBody());
        sreco.setFrame(fread);
        sreco.setFrameP(framep);

        if (tyrhs.isPos(ep)) {
          srec.setPol(true);
          srec.setPolDual(false);
          IndexedSessionRef srefd = (IndexedSessionRef) srec.getFrame().find(srec.getcch());
          sref.resetOffset();
          srefd.resetOffset();
        }

        p_cont.code = rhs;
        p_cont.frame = fwrite;
        p_cont.epnm = ep;
        return;
      }
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
