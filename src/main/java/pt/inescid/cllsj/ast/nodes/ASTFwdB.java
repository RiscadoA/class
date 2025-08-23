package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public class ASTFwdB extends ASTNode {

  String ch1;
  String ch2;

  public ASTFwdB(String _ch1, String _ch2) {
    ch1 = _ch1;
    ch2 = _ch2;
  }

  public String getCh1() {
    return ch1;
  }

  public String getCh2() {
    return ch2;
  }

  public void setCh1(String ch1) {
    this.ch1 = ch1;
  }

  public void setCh2(String ch2) {
    this.ch2 = ch2;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    throw new Exception("Unexpected call editASTInsertUse"); // never called
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    if (_ch.equals(ch1) || _ch.equals(ch2))
      throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
    return this.ASTweakeningHere(_ch, t, exp);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    ASTType t1 = ed.find(ch1);

    ASTType t2;
    try {
      t2 = eg.find(ch2);
    } catch (Exception e) {
      t2 = ed.find(ch2);
      t2 = t2.unfoldType(ep);
      if (t2 instanceof ASTWhyT) {
        ASTWhyT t = (ASTWhyT) t2;
        t2 = t.getin();
        this.getanc().ASTInsertWhyNot(ch2, t2, this);
        ed.updmove(ch2);
      } else
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "FWD!: "
                + ch2
                + " is neither unrestricted nor does it type linearly with ?");
    }

    ASTType tt1 = t1.unfoldType(ep);
    tt1 = ASTType.unfoldRec(tt1);

    if (tt1 instanceof ASTBangT) {
      ASTBangT tt1p = (ASTBangT) tt1;
      tt1 = tt1p.t.unfoldType(ep);
    } else throw new TypeError("Line " + lineno + " :" + "Fwd!: " + ch1 + " is not of ! type.");

    ASTType tt2 = t2.unfoldType(ep);
    tt2 = ASTType.unfoldRec(tt2);
    ASTType tdual = tt2.dual(ep);

    if (!tt1.equalst(tdual, ep, true, new Trail()))
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "FWD! "
              + ch1
              + ":"
              + tt1.toStr(ep)
              + " and "
              + ch2
              + ":"
              + tt2.toStr(ep)
              + " non dual types.");

    ed.upd(ch1, null);
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch1);
    s.add(ch2);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(ch1);
    s.add(ch2);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch1) ch1 = x;
    if (y == ch2) ch2 = x;
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(ch1);
    Server server = eg.find(ch2);
    channel.send(server);
  }

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    IndexedSessionRef sf1 = (IndexedSessionRef) frame.find(ch1);
    SessionClosure clos2 = (SessionClosure) frame.find(ch2);

    int doffset = sf1.getOffset();
    SessionRecord srec1 = sf1.getSessionRec();

    ASTNode c1 = srec1.getCont();
    Env<SessionField> fw = srec1.getFrame();
    Env<EnvEntry> epnw = srec1.getFrameP();

    srec1.writeSlot(clos2, doffset);
    sf1.incOffset();

    srec1.setPol(false);
    srec1.setcch(ch1);
    srec1.setFrame(frame);
    srec1.setFrameP(ep);
    c1.sam(fw, epnw);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    IndexedSessionRef sf1 = (IndexedSessionRef) frame.find(ch1);
    SessionClosure clos2 = (SessionClosure) frame.find(ch2);

    int doffset = sf1.getOffset();
    SessionRecord srec1 = sf1.getSessionRec();

    ASTNode c1 = srec1.getCont();
    Env<SessionField> fw = srec1.getFrame();
    Env<EnvEntry> epnw = srec1.getFrameP();

    srec1.writeSlot(clos2, doffset);
    sf1.incOffset();

    srec1.setPol(false);
    srec1.setcch(ch1);
    srec1.setFrame(frame);
    srec1.setFrameP(ep);

    p_cont.code = c1;
    p_cont.frame = fw;
    p_cont.epnm = epnw;

    //	    c1.sam(fw,epnw);

  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
