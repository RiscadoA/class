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
import pt.inescid.cllsj.SessionClosure;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTWhyT;

// unfolded

public class ASTBang extends ASTNode {
  String chr;
  String chi;
  ASTType type;
  ASTNode rhs;

  public ASTBang(String _chr, String _chi, ASTType _type, ASTNode _rhs) {
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

  public ASTType getType() {
    return type;
  }

  public ASTNode getRhs() {
    return rhs;
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
    if (_ch.equals(chi)) {
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
    if (_ch.equals(chi)) {
      ASTNode pushWhy = new ASTWhy(_ch, here);
      pushWhy.eg = eg;
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    return this.ASTweakeningTerm(_ch, exp);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;
    this.inferUses(chr, ed, ep);
    ASTType ty = ed.find(chr);
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRec(ty);
    if (type != null) type = type.unfoldType(ep);
    if (ty instanceof ASTBangT) {
      ASTBangT tyr = (ASTBangT) ty;
      ASTType payl = tyr.t.unfoldType(ep);
      // System.out.println("! equalst "+type+" "+payl);
      if (type != null && !type.equalst(payl, ep, true, new Trail())) {
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "! "
                + chi
                + " type mismatch; found="
                + payl.toStr(ep)
                + " expected="
                + type.toStr(ep));
      }

      type = payl; // SAM
      Set<String> s = rhs.fn(new HashSet<String>());
      s.remove(chi);
      Iterator<String> it = s.iterator();
      while (it.hasNext()) {
        String id = it.next();
        // System.out.println("DEBUG !: " + id + " is a free name!");
        ASTType tyId = null;
        try {
          tyId = ed.find(id);
        } catch (Exception e) {
        }
        if (tyId != null) {
          // System.out.println("DEBUG !: " + id + " is a in the linear context");
          tyId = tyId.unfoldType(ep);
          // tyId = ASTType.unfoldRec(tyId);
          if (tyId instanceof ASTWhyT) {
            // System.out.println("DEBUG !: " + id + " is of type ?");
            ASTWhyT t = (ASTWhyT) tyId;
            tyId = t.getin();
            this.getanc().ASTInsertWhyNot(id, tyId, this);
            ed.updmove(id);
            // System.out.println("DEBUG ! infer ? for " + id);
            // eg.crawl();
          } else throw new TypeError("Line " + lineno + " :" + "!: " + id + " is not of ?type.");
        }
      }

      ed.upd(chr, null);

      ed = new Env<ASTType>().assoc(chi, payl);
      rhs.typecheck(ed, eg, ep);

      rhs.linclose(ed, ep);

      rhs.linclose(chi, ed, ep);

    } else throw new TypeError("Line " + lineno + " :" + "!: " + chr + " is not of ! type.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(chr);
    s = rhs.fn(s);
    s.remove(chi);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(chr);
    s = rhs.fnLinear(s);
    s.remove(chi);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTType ts = type == null ? null : type.subst(e);
    ASTBang p = new ASTBang(chr, chi, ts, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chr) chr = x;
    else if (x == chi) { // we rename the bound name chs to fresh to avoid capturing name x
      String fresh = ASTNode.gensym();
      rhs.subs(fresh, chi);
      chi = fresh;
      rhs.subs(x, y);
    } else if (y != chi) rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    try {
      Channel channel = (Channel) ed.find(chr);
      Server server = new Server(chi, type, rhs, ep, eg, logger);
      // System.out.println("NEW SERVER "+chr);
      channel.send(server);
    } catch (Exception e) {
      System.out.println("CALL SERVER error" + chr);
      System.exit(0);
    }
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    IndexedSessionRef sref = (IndexedSessionRef) frame.find(chr);
    int doffset = sref.getOffset();
    SessionRecord srec = sref.getSessionRec();
    int sessionSize = type.SetOffsets(0, ep) + 1;

    if (srec.getPol()) {

      if (CLLSj.trace) {
        System.out.println("bang-op " + chr + " " + srec + " @ " + doffset);
      }

      SessionClosure clos = new SessionClosure(chi, sessionSize, type.isPos(ep), rhs, frame, ep);

      srec.writeSlot(clos, doffset);
      sref.incOffset();

      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();
      Env<EnvEntry> epn = srec.getFrameP();

      boolean pold = srec.getPolDual();

      srec.setPolDual(srec.getPol());
      srec.setPol(false); // polarity for other endpoint

      srec.setcch(chr);
      srec.setCont(null);
      srec.setFrame(frame);
      srec.setFrameP(ep);

      p_cont.code = cont;
      p_cont.frame = frm;
      p_cont.epnm = epn;

    } else {
      throw new SAMError("bang-op - " + chr);
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
