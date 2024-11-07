package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.SAM;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTCoAffineT;
import pt.inescid.cllsj.ast.types.ASTCoLBasicType;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTUsageT;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public class ASTVId extends ASTExpr {

  String ch;

  boolean linId;

  public ASTVId(String _ch) {
    ch = _ch;
  }

  public String getCh() {
    return ch;
  }

  public void setCh(String ch) {
    this.ch = ch;
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
    ASTType ty;

    // System.out.println("VID typecheck "+ch+" "+lin);

    if (lin) {
      try {
        ty = ed.find(ch);
        linId = true;
        // System.out.println("VID LIN "+ch);
        ty = ty.unfoldType(ep);
        ty = ASTType.unfoldRec(ty);
        if (ty instanceof ASTWhyT) {
          // System.out.println("VID typecheck-ASTWhy "+ch);
          ASTWhyT t = (ASTWhyT) ty;
          ty = t.getin();
          this.getanc().ASTInsertWhyNot(ch, ty, this);
          ed.updmove(ch);
          linId = false;
          return this.etypecheck(ed, eg, ep, lin);
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
        if (ty instanceof ASTCoLBasicType) {
          ed.upd(ch, null);
        }
        return ty;
      } catch (Exception e) {
        // System.out.println("VID typecheck-try-gamma "+ch);
      }
    }

    ty = eg.find(ch); // check exponential context!

    linId = false;
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);

    //	System.out.println("VID GAMMA "+ch+" : "+ ty);

    if (!(ty instanceof ASTCoLBasicType)) {
      String cho = ASTType.gensym();
      // System.out.println("VID insertCall "+cho+":"+ty);
      this.getanc().ASTInsertCall(ch, cho, ty, this);
      ed = ed.assoc(cho, ty);
      // ed.crawl();
      this.ch = cho;
      return this.etypecheck(ed, eg, ep, lin); // true was lin
    } else return ty;
    /*
      if(!lin && linId)
      throw new TypeError("Line " + lineno + " :" +"Expression id " + ch + " must be unrestricted." );
    */
  }

  public Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception {
    Value v;
    if (linId) {
      Channel channel = (Channel) ed.find(ch);
      v = (Value) channel.receive();
      return v;
    } else {
      Server server = eg.find(ch);
      if (server instanceof Value) {
        v = (Value) server;
        return v;
      } else {
        // System.out.println("EVAL server call");
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
    } else {
      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      Value sval = (Value) srec.readSlot(doffset);
      if (CLLSj.trace) {
        System.out.println("vid-op-lin " + ch + " " + srec + " @ " + doffset);
      }
      sref.incOffset();
      return sval;
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
