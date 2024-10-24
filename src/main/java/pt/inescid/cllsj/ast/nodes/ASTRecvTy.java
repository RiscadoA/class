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
import pt.inescid.cllsj.TypeClosure;
import pt.inescid.cllsj.TypeDefEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.TypeError;

public class ASTRecvTy extends ASTNode {

  String chs;
  String tyid;
  String tyidGen;
  ASTNode rhs;
  ASTType tyrhs;

  public ASTRecvTy(String _chs, String _tyid, ASTNode _rhs) {
    chs = _chs;
    tyid = _tyid;
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

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(chs)) {
      ASTNode pushWhy = new ASTWhy(_ch, here);
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    rhs = rhs.ASTweakeningOnLeaf(_ch, t, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    this.inferUses(chs, ed, ep);

    ASTType ty = ed.find(chs);

    ty = ty.unfoldType(ep);

    // System.out.println("UF TC");

    if (ty instanceof ASTRecvTT) {

      ASTRecvTT tys = (ASTRecvTT) ty;

      ASTType gs = new ASTIdT(ASTType.gensym());

      // System.out.println("\nTC RECVTy Type "+tys.getid());
      // System.out.println("TC RECVTy Proc "+tyid);
      // System.out.println("RECVTY GENSYM ="+((ASTIdT)gs).getid());

      EnvEntry gse = new TypeEntry(gs);

      Env<EnvEntry> epn = ep.assoc(((ASTIdT) gs).getid(), gse);

      // "0" -> "0"
      epn = epn.assoc(tyid, gse); // type parameter of process
      epn = epn.assoc(tys.getid(), gse); // type parameter of type
      tyidGen = ((ASTIdT) gs).getid();

      ASTType tcont = tys.getrhs().unfoldType(epn);

      tyrhs = tcont;

      ed.upd(chs, tcont);
      rhs.typecheck(ed, eg, epn);
      rhs.linclose(ed, epn);

    } else
      throw new TypeError("Line " + lineno + " :" + "RECVT: " + chs + " is not of RECVT type.");
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
    String ns = ASTType.gensym();
    e = e.assoc(tyid, new ASTIdT(ns));
    ASTRecvTy p = new ASTRecvTy(chs, ns, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chs) chs = x;

    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(chs);
    //		System.out.println("RECVTY ***"+chs+" "+channel.getId());
    ASTType recvType = (ASTType) channel.receive();
    // System.out.println("RECVDTY ****"+recvType.toStr(ep)+ " on "+chs+" "+channel.getId());
    ASTTypeDef typedef = new ASTTypeDef(tyidGen);
    typedef.setType(recvType);
    logger.info("RECV TYPE " + recvType.toStr(ep) + " on session " + channel.getId());
    rhs.runproc(ep.assoc(tyidGen, new TypeDefEntry(typedef)), ed, eg, logger);
  }

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    IndexedSessionRef sref = (IndexedSessionRef) frame.find(chs);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();
    boolean pol = srec.getPol();
    if (pol) {
      System.out.println("SAM-RECVTy " + this + " " + chs + "@" + doffset + " + ");
      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();
      Env<EnvEntry> epn = srec.getFrameP(); // !!
      srec.setPol(false);
      srec.setcch(chs);
      srec.setCont(this);
      srec.setFrame(frame);
      cont.sam(frm, epn); // !
    } else {
      // System.out.println("SAM-RECVTy "+this+" "+chs+"@"+doffset+ " - "+srec);
      TypeClosure ty = (TypeClosure) srec.readSlot(doffset);
      if (ty == null) throw new SAMError("SAM-RECVTy-read-FAILURE");
      srec.writeSlot(null, doffset);
      sref.incOffset();

      ASTTypeDef typedef = new ASTTypeDef(tyidGen);
      typedef.setType(ty.getTy());
      TypeDefEntry tde = new TypeDefEntry(typedef);
      // ep = ty.getEnv().assoc(tyidGen, new TypeDefEntry(typedef));
      ep = ep.assoc(tyidGen, tde);

      // System.out.println("ep");
      // ep.crawl();
      // System.out.println("RECVty "+ this+" "+tyidGen +" <- "+ty.getTy().toStr(ty.getEnv()));
      // System.out.println();
      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();
      Env<EnvEntry> epn = srec.getFrameP();
      // System.out.println("epn");
      // epn.crawl();
      srec.setPol(true);
      srec.setcch(chs);
      srec.setCont(rhs);
      srec.setFrame(frame);
      srec.setFrameP(ep);
      cont.sam(frm, epn);
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    IndexedSessionRef sref = (IndexedSessionRef) frame.find(chs);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();
    boolean pol = srec.getPol();
    if (pol) {
      if (CLLSj.trace) {
        System.out.println("recvty-op S [-] " + srec.getcch());
      }
      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();
      Env<EnvEntry> epn = srec.getFrameP(); // !!
      boolean pold = srec.getPolDual();
      srec.setPolDual(srec.getPol());
      srec.setPol(pold);
      srec.setcch(chs);
      srec.setCont(this);
      srec.setFrame(frame);

      p_cont.code = cont;
      p_cont.frame = frm;
      p_cont.epnm = epn;

      //	    cont.sam(frm,ep);
    } else {

      if (CLLSj.trace) {
        System.out.println("recvty-op " + chs + " " + srec + " @ " + doffset);
      }

      TypeClosure ty = (TypeClosure) srec.readSlot(doffset);

      if (ty == null) throw new SAMError("SAM-RECVTy-read-FAILURE");

      srec.writeSlot(null, doffset);
      sref.incOffset();

      ASTTypeDef typedef = new ASTTypeDef(tyidGen);

      typedef.setType(ty.getTy());
      TypeDefEntry tde = new TypeDefEntry(typedef);

      ep = ty.getEnv().assoc(tyidGen, tde);

      /*** --- previous
       *
       * ASTNode cont = srec.getCont();
       * Env<SessionField> frm = srec.getFrame();
       * Env<EnvEntry> epn = srec.getFrameP();
       *
       * srec.setPol(true);
       * srec.setcch(chs);
       * srec.setCont(rhs);
       * srec.setFrame(frame);
       * srec.setFrameP(ep);
       *
       * if (tyrhs.isPos(ep)) { // usage of types at runtime NB!!
       * srec.setPol(true);
       * srec.setPolDual(false);
       * IndexedSessionRef srefd = (IndexedSessionRef)srec.getFrame().find(srec.getcch());
       * sref.resetOffset();
       * srefd.resetOffset();
       * }
       *
       * p_cont.code = cont;
       * p_cont.frame = frm;
       * p_cont.epnm = epn;
       *
       */

      // System.out.println("recvty-trhs  "+tyrhs);

      if (tyrhs.isPos(ep)) {
        // System.out.println("recvty-trhs pos "+tyrhs);
        ASTNode cont = srec.getCont();
        Env<SessionField> frm = srec.getFrame();
        Env<EnvEntry> epn = srec.getFrameP();
        IndexedSessionRef srefd = (IndexedSessionRef) srec.getFrame().find(srec.getcch());
        sref.resetOffset();
        srefd.resetOffset();

        srec.setPol(true);
        srec.setPolDual(false);
        srec.setcch(chs);
        srec.setCont(rhs);
        srec.setFrame(frame);
        srec.setFrameP(ep);

        p_cont.code = cont;
        p_cont.frame = frm;
        p_cont.epnm = epn;
        return;
      } else {
        p_cont.code = rhs;
        p_cont.frame = frame;
        p_cont.epnm = ep;
      }
    }
  }
}
