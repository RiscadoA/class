package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.SAM;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.types.ASTCoAffineT;
import pt.inescid.cllsj.ast.types.ASTCoBasicType;
import pt.inescid.cllsj.ast.types.ASTCoLBasicType;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageT;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public class ASTVId extends ASTExpr {

  String ch;
  ASTType ty;

  boolean linId;

  public ASTVId(String _ch) {
    ch = _ch;
  }

  public ASTType getType() {
    return ty;
  }

  public String getCh() {
    return ch;
  }

  public void setCh(String ch) {
    this.ch = ch;
  }

  public boolean isLinear() {
    return linId;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {}

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (ch == y) ch = x;
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    ty = null;

    if (lin) {
      // System.out.println("lin VID typecheck " + ch);

      try {
        ty = ed.find(ch);
        linId = true;
        ty = ty.unfoldType(ep);
        ty = ASTType.unfoldRec(ty);

        if (ty instanceof ASTWhyT) {
          // System.out.println("lin VID typecheck-ASTWhy "+ch);
          ASTWhyT t = (ASTWhyT) ty;
          ty = t.getin();
          this.getanc().ASTInsertWhyNot(ch, ty, this);
          ed.updmove(ch);
          linId = false;
          return this.etypecheck(ed, eg, ep, lin);
        }

        if (ty instanceof ASTCoAffineT) {
          // System.out.println("lin VID typecheck-ASTUse "+ch);
          ASTCoAffineT tyco = (ASTCoAffineT) ty;
          ed.upd(ch, tyco.getin());
          ASTType cont = tyco.getin().unfoldType(ep);
          cont = ASTType.unfoldRec(cont);
          Boolean disposableCont =
              (cont instanceof ASTUsageT)
                  || (cont instanceof ASTCoAffineT)
                  || (cont instanceof ASTWhyT);
          this.getanc().ASTInsertUse(ch, tyco.getin(), this, disposableCont);
          return this.etypecheck(ed, eg, ep, lin);
        }
        if (ty instanceof ASTCoLBasicType) {
          // System.out.println("VID consume=" + ch + ":" + ty);
          ed.upd(ch, null);
        }
        if (ty instanceof ASTCoBasicType) {
          // System.out.println("VID CoBasic=" + ch + ":" + ty);
        }

        // System.out.println("VIDt=" + ty);
        return ty;
      } catch (Exception e) {
        // System.out.println("VID typecheck-try-gamma of ?T or linear "+ch);
      }
    }

    // must check exponential context!

    if (!lin) {
      try {
        ty = ed.find(ch);
        if (ty instanceof ASTCointT) {
          // System.out.println("VID exp typecheck-ASTCointT "+ch);
          return this.etypecheck(ed, eg, ep, true);
        }
        if (ty instanceof ASTWhyT) {
          // System.out.println("Should infer ?"+ch);
          return this.etypecheck(ed, eg, ep, true);
        }
        if (ty instanceof ASTCoAffineT) {
          // System.out.println("VID typecheck-ASTUse "+ch);
          ASTCoAffineT tyco = (ASTCoAffineT) ty;
          ed.upd(ch, tyco.getin());
          ASTType cont = tyco.getin().unfoldType(ep);
          cont = ASTType.unfoldRec(cont);
          Boolean disposableCont =
              (cont instanceof ASTUsageT)
                  || (cont instanceof ASTCoAffineT)
                  || (cont instanceof ASTWhyT);
          this.getanc().ASTInsertUse(ch, tyco.getin(), this, disposableCont);
          return this.etypecheck(ed, eg, ep, lin);
        }
      } catch (Exception e) {
      }
    }

    // eg.crawl();

    ty = eg.find(ch);

    linId = false;
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);

    if (!(ty instanceof ASTCoLBasicType)) {
      String cho = ASTType.gensym();
      // System.out.println("VID insertCall "+cho+":"+ty);
      this.getanc().ASTInsertCall(ch, cho, ty, this);
      ed = ed.assoc(cho, ty);
      this.ch = cho;
      return this.etypecheck(ed, eg, ep, lin); // true was lin
    } else return ty;
  }

  public Value eval(Env<Session> ed, Env<Server> eg) throws Exception {
    Value v;
    // System.out.println("VID-eval "+ch+" lin="+linId);
    if (linId) { // in linear context
      Session session = (Session) ed.find(ch);
      try { // session
        Channel sessionc = (Channel) session;
        // System.out.println("VID-recv "+ch+"="+sessionc);
        v = (Value) sessionc.receive();
        return v;
      } catch (Exception _) { // copyable value (int)
        // System.out.println("VID-val "+ch+"="+session);
        return (Value) session;
      }
    } else {
      Server server = eg.find(ch);
      if (server instanceof Value) {
        v = (Value) server;
        return v;
      } else {
        Channel channel = (Channel) server.call(ch);
        v = (Value) channel.receive();
        return v;
      }
    }
  }

  public Value sameval(Env<SessionField> ed) throws Exception {
    SessionField sf = ed.find(ch);
    if (sf instanceof Value) {
      if (CLLSj.trace) {
        System.out.println("vid-op-val " + ch + " " + sf);
      }
      return (Value) sf;
    } else if (sf instanceof SessionClosure) {
      SessionClosure repl = (SessionClosure) sf;
      if (CLLSj.trace) {
        System.out.println("vid-op-clos " + ch + " " + sf);
      }
      int sessionsize = repl.getSize();
      SessionRecord sreco = SessionRecord.newSessionRecord(sessionsize + 1);

      IndexedSessionRef srecfw = new IndexedSessionRef(0, sreco);
      String id = repl.getId();
      Env<SessionField> frameloc = repl.getEnv();
      Env<EnvEntry> framep = repl.getEnvP();
      Env<SessionField> fwrite = frameloc.assoc(id, srecfw);

      sreco.setPol(true);
      sreco.setPolDual(false);

      sreco.setcch("");
      sreco.setCont(null);
      sreco.setFrame(null);
      sreco.setFrameP(null);

      SAM.SAMloop(repl.getBody(), fwrite, framep);
      Value vv = (Value) sreco.readSlot(0);
      SessionRecord.freeSessionRecord(sreco);
      return vv;
    } else if (sf instanceof IndexedSessionRef) {
      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      if (CLLSj.trace) {
        System.out.println("vid-op-lin " + ch + " " + srec + " @ " + doffset);
      }
      Value sval = (Value) srec.readSlot(doffset);
      sref.incOffset();
      // if CoInt ??
      return sval;
    } else {
      if (linId) {
        LinSessionValue lsv = (LinSessionValue) sf;
        Channel channel = lsv.getLin();
        Value v = (Value) channel.receive();
        return v;
      }
      int u = 0 / 0;
      return null;
    }
  }

  @Override
  public void accept(ASTExprVisitor visitor) {
    visitor.visit(this);
  }
}
