package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTCLLType extends ASTNode {

  String id;
  ASTNode rhs;

  public ASTCLLType(String _id, ASTNode _rhs) {
    id = _id;
    rhs = _rhs;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
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

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
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
    rhs = rhs.ASTweakeningOnLeaf(_ch, t, exp);
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    boolean lin = true;
    this.eg = eg;
    ASTType ty = null;
    try {
      ty = ed.find(id);
    } catch (Exception ex) {
      ty = eg.find(id);
      lin = false;
    }
    System.out.println("\n\nclltype " + (lin ? "lin " : "exp ") + id + " : " + ty.toStr(ep));
    rhs.typecheck(ed, eg, ep);
  }

  public Set<String> fn(Set<String> s) {
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTCLLType p = new ASTCLLType(id, rhs.subst(e));
    p.lineno = lineno;
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    rhs.runproc(ep, ed, eg, logger);
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    String str = "clltype " + id + ";\n";
    return str;
  }

  public void show() {
    System.out.println(this);
    rhs.show();
  }

  static ASTType negtype = new ASTCointT();

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {}

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    p_cont.code = rhs;
    p_cont.frame = frame;
    p_cont.epnm = ep;
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
