package pt.inescid.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTDiscard extends ASTNode {
  String chr;

  public ASTDiscard(String _chr) {
    chr = _chr;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType _t, ASTNode here, Boolean disCont) throws Exception {
    throw new Exception("Unexpected call editASTInsertUse"); // never called
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    return this.ASTweakeningTerm(_ch, exp);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    ASTType ty = ed.find(chr);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, chr, ep);
    if (ty instanceof ASTCoAffineT) {
      ASTCoAffineT tyr = (ASTCoAffineT) ty;
      ed.upd(chr, null);
    } else
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "DISCARD: "
              + chr
              + " is not of COAFFINE type, found: "
              + ty.toStr(ep));
  }

  public Set<String> fn(Set<String> s) {
    s.add(chr);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(chr);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chr) chr = x;
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(chr);
    CLLSj.inc_coaff(+1);
    //        System.out.println("+DISCARD:  "+chr);

    channel.send("DISCARD");
    CLLSj.inc_coaff(-1);
    //  System.out.println("-DISCARD:  "+chr);
    logger.info("DISCARD session " + channel.getId());
  }

  public void show() {
    System.out.println(this + " " + anc);
  }

  static SessionFieldDiscard SDISC = new SessionFieldDiscard();

  public void samCDiscard(
      String chr, Channel channel, Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont)
      throws Exception {
    if (CLLSj.trace) {
      System.out.println("discard-op-lc " + chr + " " + channel);
    }
    // System.out.println("+DISCARD:  "+chr);
    SessionFieldAffine arg = (SessionFieldAffine) channel.receive();
    // System.out.println("-DISCARD:  "+chr);
    // System.out.println("++DISCARD ACK:  "+chr);
    channel.send(SDISC); // ack
    // System.out.println("--DISCARD ACK:  "+chr);
    p_cont.code = null;
    CLLSj.elapsed = System.nanoTime();
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = frame.find(chr);

    //	System.out.println("discard-op "+chr+" "+sf);

    if (sf instanceof LinSessionValue) {
      LinSessionValue lsv = (LinSessionValue) sf;
      Channel channel = lsv.getLin();
      samCDiscard(chr, channel, frame, ep, p_cont);

    } else {

      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();

      boolean pol = srec.getPol();
      // System.out.println("discard-op "+chr+" "+srec);
      if (pol) {

        throw new SAMError("discard-op + " + chr + " " + srec);
      } else {
        if (CLLSj.trace) {
          System.out.println("discard-op " + chr + " " + srec + " @ " + doffset);
        }

        SessionFieldAffine arg = (SessionFieldAffine) srec.readSlot(doffset);
        srec.writeSlot(null, doffset);
        sref.incOffset();
        if (arg == null) throw new SAMError("SAM-DISCARD-read-FAILURE");

        ASTNode cont = srec.getCont();
        Env<SessionField> frm = srec.getFrame();
        Env<EnvEntry> epn = srec.getFrameP();
        boolean pold = srec.getPolDual();

        // srec dies

        SessionRecord.freeSessionRecord(srec);

        HashMap<String, ASTType> dict = arg.getusageSet();
        if (dict.size() > 0) {
          for (Map.Entry<String, ASTType> itU : dict.entrySet()) {
            String drop = itU.getKey();
            ASTType typ = itU.getValue();
            ASTNode code = new ASTDiscard(drop); // affine already set in frame

            srec.setCont(null);
            srec.setcch(chr);
            srec.setFrame(frame);
            srec.setFrameP(ep);

            p_cont.code = cont;
            p_cont.frame = frm;
            p_cont.epnm = epn;
            code.samL(frm, epn, p_cont);
          }
        }
        p_cont.code = null;
        CLLSj.elapsed = System.nanoTime();
      }
    }
  }
}
