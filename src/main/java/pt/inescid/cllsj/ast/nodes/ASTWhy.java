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
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTCoBasicType;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public class ASTWhy extends ASTNode {
  String ch;
  ASTNode rhs;
  ASTType __type;

  public ASTWhy(String _ch, ASTNode _rhs) {
    this(_ch, null, _rhs);
  }

  public ASTWhy(String _ch, ASTType _t, ASTNode _rhs) {
    ch = _ch;
    rhs = _rhs;
    __type = _t;
  }

  @Override
  public String getSubjectCh() {
    return ch;
  }

  public String getCh() {
    return ch;
  }

  public void setCh(String ch) {
    this.ch = ch;
  }

  public ASTNode getRhs() {
    return rhs;
  }

  public ASTType getType() {
    if (__type == null) {
      throw new RuntimeException("ASTWhy's type is null, wasn't typecheck called?");
    }
    return __type;
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

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
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
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);
    if (ty instanceof ASTWhyT) {
      ASTWhyT tyr = (ASTWhyT) ty;
      ed.upd(ch, null);
      __type = tyr.getin().unfoldType(ep);
      //		eg=eg.assoc(ch, type);
      eg = eg.assoc(ch, __type);
      rhs.typecheck(ed, eg, ep);
    } else if (ty instanceof ASTCoBasicType) {
      ASTCoBasicType tyr = (ASTCoBasicType) ty;
      ed.upd(ch, null);
      __type = tyr.lift();
      eg = eg.assoc(ch, __type);
      rhs.typecheck(ed, eg, ep);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "?: " + ch + " is neither of ? or basic co-type.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(ch);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTWhy p = new ASTWhy(ch, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) ch = x;

    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(ch);
    Server server = (Server) channel.receive();

    logger.info("Server activated on session " + channel.getId());
    rhs.runproc(ep, ed, eg.assoc(ch, server), logger);
  }

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {

    IndexedSessionRef sref = (IndexedSessionRef) frame.find(ch);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();

    boolean pol = srec.getPol();
    if (pol) {
      // polarity +
      System.out.println("SAM-WHY " + ch + "@" + doffset + " + WAIT");
      System.exit(0);
    } else {
      // polarity -
      // System.out.println("SAM-WHY "+ch+"@"+doffset+ " - ");
      SessionField sf = srec.readSlot(doffset);
      sref.incOffset();
      if (sf == null) throw new SAMError("SAM-WHY-read-FAILURE");
      srec.writeSlot(null, doffset); // reset linear value
      frame.upd(ch, sf);
      rhs.sam(frame, ep);
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    IndexedSessionRef sref = (IndexedSessionRef) frame.find(ch);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();
    boolean pol = srec.getPol();

    if (pol) {
      int i = 9 / 0;
      /*
         if (CLLSj.trace) {
      System.out.println("why?-op [-] "+ srec.getcch());
         }
         ASTNode cont = srec.getCont();
         Env<SessionField> frm  = srec.getFrame();
         Env<EnvEntry> epn  = srec.getFrameP();
         boolean pold = srec.getPolDual();

         srec.setPolDual(srec.getPol());   // this session dies so don't really care
         srec.setPol(pold);

         srec.setCont(this); // may this should be GC'ed ?
         srec.setcch(ch);
         srec.setFrame(frame);
         srec.setFrameP(ep);

         p_cont.code = cont;
         p_cont.frame = frm;
         p_cont.epnm = epn;
         return;
         */
    } else {
      if (CLLSj.trace) {
        System.out.println("why?-op " + ch + " " + srec + " @ " + doffset);
      }

      SessionField sf = srec.readSlot(doffset);
      // System.out.println("why?-op "+sf);
      if (sf == null) throw new SAMError("SAM-WHY-read-FAILURE");
      srec.writeSlot(null, doffset);
      sref.incOffset();
      // frame.upd(ch, sf);
      frame = frame.assoc(ch, sf);
      p_cont.code = rhs;
      p_cont.frame = frame;
      p_cont.epnm = ep;
      SessionField sf0 = frame.find(ch);
      if (sf0 != sf) throw new SAMError("UPDATE-WHY");
      SessionRecord.freeSessionRecord(srec);
      return;
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
