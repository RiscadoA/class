package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LabelSet;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.MVar;
import pt.inescid.cllsj.Pair;
import pt.inescid.cllsj.SAM;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionFieldAffine;
import pt.inescid.cllsj.SessionFieldUnfold;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTAffineT;
import pt.inescid.cllsj.ast.types.ASTCellBLT;
import pt.inescid.cllsj.ast.types.ASTCellBT;
import pt.inescid.cllsj.ast.types.ASTCellLT;
import pt.inescid.cllsj.ast.types.ASTCellT;
import pt.inescid.cllsj.ast.types.ASTCoRecT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTOneT;
import pt.inescid.cllsj.ast.types.ASTRecT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTintT;

public class ASTFwd extends ASTNode {

  String ch1;
  String ch2;
  ASTType typeCh2;

  public ASTFwd(String _ch1, String _ch2) {
    ch1 = _ch1;
    ch2 = _ch2;
  }

  public String getCh1() {
    return ch1;
  }

  public void setCh1(String ch1) {
    this.ch1 = ch1;
  }

  public String getCh2() {
    return ch2;
  }

  public void setCh2(String ch2) {
    this.ch2 = ch2;
  }

  public ASTType getCh2Type() {
    return typeCh2;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    throw new Exception("Unexpected call editASTInsertUse"); // never called
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    if (_ch.equals(ch1) || _ch.equals(ch2))
      throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
    return this.ASTweakeningHere(_ch, typ, exp);
  }

  private Pair<ASTType, ASTType> fwdInferRec(
      ASTNode here, String ch1, ASTType t1, String ch2, ASTType t2, Env<EnvEntry> ep, boolean rec)
      throws Exception {
    //	ASTType t1n = ASTType.unfoldRec(t1);

    ASTType t1n = (t1 instanceof ASTRecT) ? ((ASTRecT) t1).getin() : ((ASTCoRecT) t1).getin();
    ASTNode parent = here.getanc();
    Function<ASTNode, ASTNode> f =
        (ASTNode x) -> {
          ASTUnfold n = new ASTUnfold(ch1, x);
          n.rec = rec;
          n.setrhs(t1n);
          // System.out.println("FWD infer UNFOLD "+ch1+" "+rec+" "+x);
          return n;
        };
    // System.out.println("FWD infer REC UNFOLD PARENT "+parent+" "+t1.toStr(ep)+" "+t1n.toStr(ep));
    parent.ASTInsertPipe(f, here);
    return fwdInfer(here, ch1, t1n, ch2, t2, ep);
  }

  // unused
  private Pair<ASTType, ASTType> fwdInferAffine(
      ASTNode here, String ch1, ASTType t1, String ch2, ASTType t2, Env<EnvEntry> ep)
      throws Exception {
    ASTType t1n = ((ASTAffineT) t1).getin();
    ASTNode parent = here.getanc();
    Function<ASTNode, ASTNode> f =
        (ASTNode x) -> {
          ASTAffine n = new ASTAffine(ch1, x);
          n.contType = t1n;
          // System.out.println("FWD infer AFFINE "+ch1);
          return n;
        };
    // System.out.println("FWD infer AFFINE PARENT "+parent);
    parent.ASTInsertPipe(f, here);
    return fwdInfer(here, ch1, t1n, ch2, t2, ep);
  }

  private Pair<ASTType, ASTType> fwdInfer(
      ASTNode here, String ch1, ASTType t1, String ch2, ASTType t2, Env<EnvEntry> ep)
      throws Exception {
    if (t1.equalst(t2.dual(ep), ep, true, new Trail())) return new Pair(t1, t2);
    // System.out.println("fwdInfer t1 = "+t1.toStr(ep)+" t2 = "+t2.toStr(ep));
    if (t1 instanceof ASTRecT) {
      try {
        return fwdInferRec(here, ch1, t1, ch2, t2, ep, true);
      } catch (Exception e1) {
      }
    }
    if (t2 instanceof ASTRecT) {
      try {
        return fwdInferRec(here, ch2, t2, ch1, t1, ep, true);
      } catch (Exception e2) {
      }
    }
    if (t1 instanceof ASTCoRecT) {
      try {
        return fwdInferRec(here, ch1, t1, ch2, t2, ep, false);
      } catch (Exception e1) {
      }
    }
    if (t2 instanceof ASTCoRecT) {
      try {
        return fwdInferRec(here, ch2, t2, ch1, t1, ep, false);
      } catch (Exception e2) {
      }
    }
    throw new TypeError(
        "Line "
            + lineno
            + " :"
            + "FWD "
            + ch1
            + ":"
            + t1.toStr(ep)
            + " and "
            + ch2
            + ":"
            + t2.toStr(ep)
            + " non dual types.");
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    ASTType t1 = ed.find(ch1);
    ASTType t2 = ed.find(ch2);

    // System.out.println("fwd0 "+ch2+":"+t2 +" "+ ch1+":"+t1);

    ASTType tt1 = t1.unfoldType(ep);
    ASTType t1s = tt1;
    tt1 = ASTType.unfoldRec(tt1);
    // if (!t1s.equalst(tt1,ep, true, new Trail())) System.out.println("TT1 === "+t1s+" "+tt1);
    ASTType tt2 = t2.unfoldType(ep);
    ASTType t2s = tt2;
    tt2 = ASTType.unfoldRec(tt2);
    // if (!t2s.equalst(tt2,ep, true, new Trail())) System.out.println("TT2 === "+t2s+" "+tt2);

    // System.out.println("fwd1 "+ch2+":"+t2s +" "+ ch1+":"+t1s);

    // dualize inference properly
    // tt1 = ASTType.unfoldRecInfer(tt1,  this, ch1, ep);
    // tt2 = ASTType.unfoldRecInfer(tt2, this, ch2, ep);

    //	typeCh2 = tt2;
    //      ASTType tdual =	tt2.dual(ep);

    typeCh2 = t2s;
    ASTType tdual = t2s.dual(ep);

    // .unfoldType(ep);
    // ASTType tdual =	tt1;

    // System.out.println("fwd "+ch2+":"+tt2+"->"+typeCh2 + " " +ch1+":"+tt1+" -> "+tdual);
    // System.out.println("fwd "+ch2+":"+t2s+"->"+typeCh2 + " " +ch1+":"+t1s+" -> "+tdual);

    ASTNode parentc = this.getanc();

    Pair<ASTType, ASTType> p2 = fwdInfer(this, ch1, t1s, ch2, t2s, ep);

    t1s = p2.fst;
    tdual = p2.snd.dual(ep);

    if (!t1s.equalst(tdual, ep, true, new Trail())) {

      if (true)
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "FWD "
                + ch1
                + ":"
                + tt1.toStr(ep)
                + " and "
                + ch2
                + ":"
                + tt2.toStr(ep)
                + " non dual types.");

    } else {
      ed.upd(ch1, null);
      ed.upd(ch2, null);
    }
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
    if (y == ch2)
      ;
    ch2 = x;
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {

    // when forwarding a cell, make sure the cell is on channel ch1
    ASTType typeCh2i = ASTType.unfoldRec(typeCh2);
    // was unfoldtype, attention, concurrent interpreter do not unfold op!
    //	ASTType typeCh2i = typeCh2.unfoldType(ep);
    String ch1i, ch2i;
    // System.out.println("FWD START "+ch1+" : "+typeCh2i.dual(ep).toStr(ep)+" "+ch2+" :
    // "+typeCh2i.toStr(ep));

    if (typeCh2i instanceof ASTCellT
        || typeCh2i instanceof ASTCellLT
        || typeCh2i instanceof ASTCellBT
        || typeCh2i instanceof ASTCellBLT) {
      ch1i = ch2;
      ch2i = ch1;
      // System.out.println("FWD SWITCH "+ch1i+" : "+typeCh2i.toStr(ep)+" "+ch2i+" :
      // "+typeCh2i.dual(ep).toStr(ep));
    } else {
      ch1i = ch1;
      ch2i = ch2;
    }

    // System.out.println("FWD "+ch1i+" : "+ch2i+" : "+typeCh2i);

    try {

      // System.out.println("Type fwd ch2 "+ typeCh2i);
      if (typeCh2i instanceof ASTintT) {
        LinSession session2 = (LinSession) ed.find(ch2i);
        session2.send(ed.find(ch1i));
        return;
      } else {
        if (typeCh2i instanceof ASTCointT) {
          LinSession session1 = (LinSession) ed.find(ch1i);
          session1.send(ed.find(ch2i));
          return;
        }
      }
      LinSession session1 = (LinSession) ed.find(ch1i);
      LinSession session2 = (LinSession) ed.find(ch2i);

      // System.out.println("Set fwd -"+ session1 + " -> " + session2);
      session1.setFwdSession(session2);
      // System.out.println("Set fwd +"+ session1 + " -> " + session2);

      logger.info("Set fwd -" + session1 + " -> " + session2);

    } catch (TypeError ee) {
      System.out.println("FWD ERR " + ee.msg);
      throw new Exception();
    }
  }

  private void merge_sessions(
      String chw,
      IndexedSessionRef sfw,
      SessionRecord srecw,
      int offsetw,
      String chr,
      IndexedSessionRef sfr,
      SessionRecord srecr,
      int offsetr,
      Env<EnvEntry> ep)
      throws Exception {

    // System.out.println("concat reader "+ chr+"("+srecr+")" + " with writer "+chw+"("+srecw+")");

    ASTNode c1 = srecw.getCont();
    Env<SessionField> fw = srecw.getFrame();
    Env<EnvEntry> epnw = srecw.getFrameP();

    ASTNode c2 = srecr.getCont();
    Env<SessionField> fr = srecr.getFrame();
    Env<EnvEntry> epnr = srecr.getFrameP();

    // System.out.println("fr"); fr.crawl();
    // System.out.println("fw"); fw.crawl();

    IndexedSessionRef sref2o = (IndexedSessionRef) fr.find(srecr.getcch());
    int doff2 = sref2o.getOffset();

    int ix = offsetr;
    int bx = offsetw;
    while (ix < doff2) {
      SessionField v = srecr.readSlot(ix);
      //	    System.out.println("copy R->W "+ix+"->"+bx+" "+v);
      srecw.writeSlot(v, bx);
      sfw.incOffset();
      ix++;
      bx++;
    }
    sref2o.UpdSessionRefInplace(srecw, bx);
    srecw.setPol(false);
    srecw.setcch(srecr.getcch());
    // System.out.println(srecw+".setcch(chr) = "+srecr.getcch());
    srecw.setCont(c2);
    srecw.setFrame(fr);
    srecw.setFrameP(epnr);
    c1.sam(fw, epnw);
  }

  private void samfwdV(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf1 = frame.find(ch1);
    SessionField sf2 = frame.find(ch2);

    if (CLLSj.trace) {
      System.out.println("fwd-op vc " + ch2 + " <- " + ch1 + " " + sf2 + " " + sf1);
    }

    IndexedSessionRef sf = (IndexedSessionRef) sf2;
    SessionRecord srec = sf.getSessionRec();
    boolean pol1 = srec.getPol();
    int doffset = sf.getOffset();

    srec.writeSlot(sf1, doffset);
    sf.incOffset();

    ASTNode cont = srec.getCont();
    Env<SessionField> frm = srec.getFrame();
    Env<EnvEntry> epn = srec.getFrameP();
    srec.setcch(ch1);
    srec.setPol(false);
    srec.setCont(null);
    srec.setFrame(frame);
    srec.setFrameP(ep);
    p_cont.code = cont;
    p_cont.frame = frm;
    p_cont.epnm = epn;
  }

  public void seq2con(
      String ch1i,
      SessionField sv1,
      String ch2i,
      SessionField sv2,
      Env<EnvEntry> ep,
      Env<SessionField> frame,
      SAMCont p_cont)
      throws Exception {
    IndexedSessionRef sf1 = (IndexedSessionRef) sv1;
    LinSessionValue lsv2 = (LinSessionValue) sv2;
    LinSession channel2 = lsv2.getLin();

    // sf1 = sequential queue
    // lsv2 = blocking channel (holds positive endpoint, will write)
    // corresponds to (SFwdMc)

    SessionRecord srec1 = sf1.getSessionRec();
    boolean pol1 = srec1.getPol();
    int doffset1 = sf1.getOffset();

    ASTNode cont = srec1.getCont();
    Env<SessionField> frm = srec1.getFrame();
    Env<EnvEntry> epn = srec1.getFrameP();
    String wch = srec1.getcch();

    if (CLLSj.trace)
      System.out.println(
          "fwdc-op-sc "
              + ch2i
              + " "
              + channel2
              + " "
              + ch1i
              + " "
              + sf1
              + " -> "
              + wch
              + " ("
              + frm.find(wch)
              + ")");

    int doffset2 = ((IndexedSessionRef) (frm.find(wch))).getOffset();

    // flush linear session to process

    for (int vix = doffset1; vix < doffset2; vix++) {
      SessionField v = srec1.readSlot(vix);
      if (v instanceof SessionClosure) {
        // System.out.println("To flush v = "+ v);
        SessionClosure vclos = (SessionClosure) v;
        ASTSend sendn = new ASTSend(wch, vclos.getId(), null, vclos.getBody(), cont);
        sendn.tys_lhs = new ASTOneT(); // dummy place holder
        cont.setanc(sendn);
        cont = sendn;
      } else if (v instanceof LabelSet) {
        // System.out.println("To flush v = "+ v);
        String lab = ((LabelSet) v).getLabel();
        ASTSelect sel = new ASTSelect(wch, lab, cont);
        cont.setanc(sel);
        cont = sel;
      } else if (v instanceof SessionFieldUnfold) {
        // System.out.println("To flush v = "+ v);
        ASTUnfold unf = new ASTUnfold(wch, cont);
        cont.setanc(unf);
        cont = unf;
      } else if (v instanceof SessionFieldAffine) {
        ASTAffine aff = new ASTAffine(wch, cont);
        cont.setanc(aff);
        cont = aff;
      } else {
        System.out.println("fwd lin to conc = " + v);
        int n = 0 / 0;
      }
    }

    SessionRecord.freeSessionRecord(srec1);

    frm.upd(wch, lsv2);

    p_cont.code = cont;
    p_cont.frame = frm;
    p_cont.epnm = epn;
  }

  public void con2seq(
      String ch1i,
      SessionField sv1,
      String ch2i,
      SessionField sv2,
      Env<EnvEntry> ep,
      Env<SessionField> frame,
      SAMCont p_cont)
      throws Exception {
    LinSessionValue lsv1 = (LinSessionValue) sv1;
    IndexedSessionRef sf2 = (IndexedSessionRef) sv2;

    LinSession channel1 = lsv1.getLin();

    // sf2 = sequential queue (holds positive endpoint, is in write mode, suspended process will
    // read)
    // lsv1 = blocking channel (holds negative endpoint, will read)
    // <P> ch1- | fwd -ch1 ch2+ | ch2<q,Q>(rhc) ==>  <q>;P | rhc | Q !!
    // corresponds to (SFwdMc)

    SessionRecord srec2 = sf2.getSessionRec();
    boolean pol2 = srec2.getPol();
    int doffset2 = sf2.getOffset();

    ASTNode contq = srec2.getCont();
    Env<SessionField> frm = srec2.getFrame();
    Env<EnvEntry> epn = srec2.getFrameP();
    String rch = srec2.getcch();

    if (CLLSj.trace)
      System.out.println(
          "fwdc-op-cs "
              + ch1i
              + " "
              + channel1
              + " "
              + ch2i
              + " "
              + sf2
              + " -> "
              + rch
              + " ("
              + frm.find(rch)
              + ") ");

    int doffset1 = ((IndexedSessionRef) (frm.find(rch))).getOffset();

    // if (true || rch.equals(ch1i)) {

    // ( ch1<q> P |rch| Q )  (if rch = ch1)

    // ( P |ch1| rzh<q>;fwd ch1 rzh ) |rzh-rch| Q

    String rzh = ASTNode.gensym();
    ASTNode cont = new ASTFwd(ch1i, rzh);
    ((ASTFwd) cont).typeCh2 = typeCh2;

    for (int vix = doffset1; vix < doffset2; vix++) {
      SessionField v = srec2.readSlot(vix);
      if (v instanceof SessionClosure) {
        // System.out.println("To flush v = "+ v);
        SessionClosure vclos = (SessionClosure) v;
        ASTSend sendn = new ASTSend(rzh, vclos.getId(), null, vclos.getBody(), cont);
        sendn.tys_lhs = new ASTOneT(); // dummy place holder
        cont.setanc(sendn);
        cont = sendn;
      } else if (v instanceof LabelSet) {
        // System.out.println("To flush v = "+ v);
        String lab = ((LabelSet) v).getLabel();
        ASTSelect sel = new ASTSelect(rzh, lab, cont);
        cont.setanc(sel);
        cont = sel;
      } else if (v instanceof SessionFieldUnfold) {
        // System.out.println("To flush v = "+ v);
        ASTUnfold unf = new ASTUnfold(rzh, cont);
        cont.setanc(unf);
        cont = unf;
      } else if (v instanceof SessionFieldAffine) {
        ASTAffine aff = new ASTAffine(rzh, cont);
        cont.setanc(aff);
        cont = aff;
      } else {
        // unreachable
        System.out.println("unexpected queue value = " + v);
        int n = 0 / 0;
      }
    }

    LinSessionValue ns = new LinSessionValue(rzh);

    Env<SessionField> edCut1 = frame.assoc(rzh, ns);
    Env<SessionField> edCut2 = frm.assoc(rch, ns);

    final ASTNode contf = contq;

    CLLSj.threadPool.submit(
        new Runnable() {
          public void run() {
            try {
              SAM.SAMloop(contf, edCut2, ep);
            } catch (Exception e) {
              e.printStackTrace(System.out);
            }
          }
        });

    p_cont.code = cont;
    p_cont.frame = edCut1;
    p_cont.epnm = ep;

    SessionRecord.freeSessionRecord(srec2);

    /*
       } else {

       // ( rhc<q> fwd rch ch1 |ch1| P ) |rch| Q  (if rch != ch1)

       String cposch = ch2i;
       System.out.println("To cposch = "+ cposch);
       ASTNode cont = this;

       for (int vix=doffset1;vix<doffset2;vix++)
       {
       SessionField v = srec2.readSlot(vix);
       if (v instanceof SessionClosure) {
       System.out.println("To flush v = "+ v);
       SessionClosure vclos = (SessionClosure)v;
       ASTSend sendn = new ASTSend(cposch,vclos.getId(),null,vclos.getBody(),cont);
       sendn.tys_lhs = new ASTOneT(); // dummy place holder
       cont.setanc(sendn);
       cont = sendn;
       } else if (v instanceof LabelSet) {
       System.out.println("To flush v = "+ v);
       String lab = ((LabelSet)v).getLabel();
       ASTSelect sel = new ASTSelect(cposch,lab,cont);
       cont.setanc(sel);
       cont = sel;
       } else if (v instanceof SessionFieldUnfold) {
       System.out.println("To flush v = "+ v);
       ASTUnfold unf = new ASTUnfold(cposch,cont);
       cont.setanc(unf);
       cont = unf;
       } else {
       // unreachable
       System.out.println("unexpected queue value = "+ v);
       int n = 0/0;
       }

       }

       LinSessionValue ns = new LinSessionValue(cposch);

       Env<SessionField> edCut1 = frame.assoc(cposch,ns);
       Env<SessionField> edCut2 = frm.assoc(rch,ns);
       edCut1.crawl();
       edCut2.crawl();
       final ASTNode contf = contq;
       System.out.println("contq ="+ contq);

       CLLSj.threadPool.submit(
       new Runnable(){
       public void run(){ try {
       SAM.SAMloop(contf,edCut2, ep);
       } catch (Exception e) {e.printStackTrace(System.out);} }
       });

       System.out.println("p_cont.code ="+ cont);
       p_cont.code = cont;
       p_cont.frame = edCut1;
       p_cont.epnm = ep;

       SessionRecord.freeSessionRecord(srec2);
       }
    */
  }

  public void SAMLSfwd(Env<EnvEntry> ep, Env<SessionField> frame, SAMCont p_cont) throws Exception {

    ASTType typeCh2i = typeCh2.unfoldType(ep);
    String ch1i, ch2i;

    if (typeCh2i instanceof ASTCellT
        || typeCh2i instanceof ASTCellLT
        || typeCh2i instanceof ASTCellBT
        || typeCh2i instanceof ASTCellBLT) {
      ch1i = ch2;
      ch2i = ch1;
    } else {
      ch1i = ch1;
      ch2i = ch2;
    }

    if (CLLSj.trace) System.out.println("fwdc-op-cc " + ch2i + " " + ch1i);

    try {

      SessionField sv1 = frame.find(ch1i);
      SessionField sv2 = frame.find(ch2i);

      if (sv1 instanceof IndexedSessionRef && !(sv2 instanceof IndexedSessionRef)) {

        seq2con(ch1i, sv1, ch2i, sv2, ep, frame, p_cont);

      } else if ((!(sv1 instanceof IndexedSessionRef)) && sv2 instanceof IndexedSessionRef) {

        con2seq(ch1i, sv1, ch2i, sv2, ep, frame, p_cont);
      } else {
        LinSessionValue lsv1 = (LinSessionValue) sv1;
        LinSession channel1 = lsv1.getLin();
        LinSessionValue lsv2 = (LinSessionValue) sv2;
        LinSession channel2 = lsv2.getLin();

        if (CLLSj.trace)
          System.out.println("fwdc-op-cc " + ch2 + " " + channel2 + " " + ch1 + " " + channel1);

        channel1.setFwdSession(channel2);

        p_cont.code = null;
      }
    } catch (TypeError ee) {
      System.out.println("FWD ERR " + ee.msg);
      throw new Exception();
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    if (!typeCh2.isPos(ep)) {
      String chs = ch1;
      ch1 = ch2;
      ch2 = chs;
      typeCh2 = typeCh2.dual(ep);
    }

    SessionField sf11 = frame.find(ch1);
    SessionField sf12 = frame.find(ch2);
    // System.out.println("fwd-op lc "+ ch2 + " <- " + ch1 + " " +sf12 + " "+sf11);

    if (sf11 instanceof LinSessionValue || sf12 instanceof LinSessionValue) {
      SAMLSfwd(ep, frame, p_cont);
      return;
    }

    if (sf11 instanceof Value || sf11 instanceof MVar) {
      this.samfwdV(frame, ep, p_cont);
    } else {

      IndexedSessionRef sf1 = (IndexedSessionRef) sf11;
      SessionRecord srec1 = sf1.getSessionRec();
      boolean pol1 = srec1.getPol();
      int doffset1 = sf1.getOffset();
      IndexedSessionRef sf2 = (IndexedSessionRef) sf12;
      int doffset2 = sf2.getOffset();
      SessionRecord srec2 = sf2.getSessionRec();
      boolean pol2 = srec2.getPol();

      if (typeCh2.isPos(ep)) // always true typeCh2 positive type
      {
        // ch1 is negative type
        if (pol1) { // but in write mode, switch to other end-point to allow writer
          if (CLLSj.trace) {
            System.out.println("fwd-op S [-] " + ch2 + " " + pol2 + " " + ch1 + " " + pol1);
          }
          ASTNode cont = srec1.getCont();
          Env<SessionField> frm = srec1.getFrame();
          Env<EnvEntry> epn = srec1.getFrameP();
          srec1.setcch(ch1);
          srec1.setPol(false);
          // srec1.setPolDual(true); // ??
          srec1.setCont(this);
          srec1.setFrame(frame);
          srec1.setFrameP(ep);
          p_cont.code = cont;
          p_cont.frame = frm;
          p_cont.epnm = epn;

        } else {
          if (CLLSj.trace) {
            System.out.println("fwd-op lc " + ch2 + " <- " + ch1 + " " + srec2 + " " + srec1);
          }
          concat_sessionsL(ch2, sf2, srec2, doffset2, ch1, sf1, srec1, doffset1, ep, p_cont);
        }
      } else {
        throw new SAMError("fwd-op - " + ch2 + " " + ch1);
      }
    }
  }

  private void concat_sessionsL(
      String chw,
      IndexedSessionRef sfw,
      SessionRecord srecw,
      int offsetw,
      String chr,
      IndexedSessionRef sfr,
      SessionRecord srecr,
      int offsetr,
      Env<EnvEntry> ep,
      SAMCont p_cont)
      throws Exception {
    ASTNode c1 = srecw.getCont();
    Env<SessionField> fw = srecw.getFrame();
    Env<EnvEntry> epnw = srecw.getFrameP();

    ASTNode c2 = srecr.getCont();
    Env<SessionField> fr = srecr.getFrame();
    Env<EnvEntry> epnr = srecr.getFrameP();

    SessionField sref2o = (SessionField) fr.find(srecr.getcch());

    if (sref2o instanceof MVar) {
      MVar v = (MVar) sref2o;
      srecw.writeSlot(v, offsetw);
      // System.out.println("move "+v+" -> "+offsetw);
      sfw.incOffset();
    } else {
      IndexedSessionRef sref2ois = (IndexedSessionRef) sref2o;
      SessionRecord sref2or = sref2ois.getSessionRec();
      int doff2 = sref2ois.getOffset();
      int ix = offsetr;
      int bx = offsetw;
      // System.out.println(offsetr + " -> "+offsetw);
      while (ix < doff2) {
        SessionField v = srecr.readSlot(ix);
        srecw.writeSlot(v, bx);
        // System.out.println("move "+v+" "+ix+" -> "+bx);
        sfw.incOffset();
        ix++;
        bx++;
      }
      sref2ois.UpdSessionRefInplace(srecw, bx);
    }
    srecw.setPol(false); // THIS MUST BE CHECKED
    srecw.setPolDual(true); // THIS MUST BE CHECKED

    srecw.setcch(srecr.getcch());

    srecw.setCont(c2);
    srecw.setFrame(fr);
    srecw.setFrameP(epnr);

    p_cont.code = c1;
    p_cont.frame = fw;
    p_cont.epnm = epnw;

    SessionRecord.freeSessionRecord(srecr);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
