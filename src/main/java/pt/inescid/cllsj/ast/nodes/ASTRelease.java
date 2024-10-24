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
import pt.inescid.cllsj.MVar;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.TypeError;

public class ASTRelease extends ASTNode {
  String chr;

  public ASTRelease(String _chr) {
    chr = _chr;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    throw new Exception("Unexpected call editASTInsertUse"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    return this.ASTweakeningTerm(_ch, exp);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    //	this.inferUses(chr,ed,ep);

    ASTType ty = ed.find(chr);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, chr, ep);

    if (ty instanceof ASTUsageT) {
      ASTUsageT tyr = (ASTUsageT) ty;
      ed.upd(chr, null);
    } else if (ty instanceof ASTUsageBT) {
      ASTUsageBT tyr = (ASTUsageBT) ty;
      ed.upd(chr, null);
    } else
      throw new TypeError(
          "Line " + lineno + " :" + "RELEASE: " + chr + " is neither of USAGE nor of USAGE! type.");
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

  public void subs(String x, String y) {
    if (y == chr) chr = x;
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Cell cell = (Cell) ed.find(chr);
    // System.out.println("RELEASE REQ "+chr+" "+cell);
    cell.free();
    CLLSj.elapsed = System.nanoTime();
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    SessionField sf = (SessionField) frame.find(chr);

    if (sf instanceof MVar) {
      MVar v = (MVar) sf;
      MVar.dropMVar(v);
      if (CLLSj.trace) {
        System.out.println("drop-op " + chr + " " + v);
      }
    } else {
      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      if (!srec.getPol()) {
        MVar v = (MVar) srec.readSlot(doffset);
        if (CLLSj.trace) {
          System.out.println("drop-op " + chr + " " + srec + " @ " + doffset + " " + v);
        }
        sref.incOffset();
        MVar.dropMVar((MVar) v);
        SessionRecord.freeSessionRecord(srec);
      } else throw new SAMError("drop-op - " + chr);
    }
    p_cont.code = null;
    CLLSj.elapsed = System.nanoTime();
  }
}
