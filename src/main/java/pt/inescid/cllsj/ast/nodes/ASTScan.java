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
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.VBool;
import pt.inescid.cllsj.VInt;
import pt.inescid.cllsj.VString;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTBasicType;
import pt.inescid.cllsj.ast.types.ASTLboolT;
import pt.inescid.cllsj.ast.types.ASTLintT;
import pt.inescid.cllsj.ast.types.ASTLstringT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTScan extends ASTNode {
  String ch;
  ASTType type;

  public ASTScan(String _ch, ASTType _type) {
    ch = _ch;
    type = _type;
  }

  public String getCh() {
    return ch;
  }

  public void setCh(String ch) {
    this.ch = ch;
  }

  public ASTType getType() {
    return type;
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("ASTupdCont: call not expected");
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    anc.ASTInsertUse(ch, t, this, disCont); // insert above up
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    anc.ASTInsertCall(ch, cho, t, this); // insert above up
  }

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.inferUses(ch, ed, ep);

    ASTType ty = ed.find(ch);
    if (type != null && !type.equalst(ty, ep, true, new Trail())) {
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "SCAN "
              + ch
              + " type mismatch: expected "
              + type.toStr(ep)
              + ", found "
              + ty.toStr(ep));
    }
    ed.upd(ch, null);
    type = ty;

    ASTType t = type;
    if (t instanceof ASTBangT) {
      t = ((ASTBangT)t).getin();
    }

    if (!(t instanceof ASTBasicType)) {
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "SCAN "
              + ch
              + " not of (optionally exponential) basic type (found "
              + type.toStr(ep)
              + ")");
    }
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return new ASTScan(ch, type == null ? null : type.subst(e));
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (ch.equals(x)) {
      ch = y; // Substitute ch by y
    }
  }

  private Value valueFromStdin(Env<EnvEntry> ep) throws Exception {
    Scanner scanner = new Scanner(System.in);
    try {
      if (type instanceof ASTLintT) {
        return new VInt(scanner.nextInt());
      } else if (type instanceof ASTLboolT) {
        return new VBool(scanner.nextBoolean());
      } else if (type instanceof ASTLstringT) {
        return new VString(scanner.nextLine());
      } else {
        throw new TypeError("Line " + lineno + " :" + "SCAN: unsupported type " + type.toStr(ep));
      }
    } finally {
      scanner.close();
    }
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(ch);
    channel.send(valueFromStdin(ep));
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    Value v = valueFromStdin(ep);

    SessionField sf = frame.find(ch);

    if (sf instanceof IndexedSessionRef) {

      IndexedSessionRef sref = (IndexedSessionRef) frame.find(ch);
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();

      if (CLLSj.trace) {
        System.out.println("coexpr-op " + ch + " " + srec + " @ " + doffset);
      }

      srec.writeSlot(v, doffset);
      sref.incOffset();

      ASTNode cont = srec.getCont();
      Env<SessionField> frm = srec.getFrame();
      Env<EnvEntry> epn = srec.getFrameP();

      srec.setPol(false);
      srec.setPolDual(true); // ended (don't care)
      srec.setFrame(frame);
      srec.setFrameP(ep);

      p_cont.code = cont;
      p_cont.frame = frm;
      p_cont.epnm = epn;

    } else {
      int u = 0 / 0;
    }
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    return "scan (" + ch + ")";
  }

  public void show() {
    System.out.println(this);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
