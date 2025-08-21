import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTUnreachable extends ASTNode {
  String ch;

  public ASTUnreachable(String _ch) {
    ch = _ch;
  }

  public String getCh() {
    return ch;
  }

  public void setCh(String ch) {
    this.ch = ch;
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
    ed.upd(ch, null);
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return new ASTUnreachable(ch);
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (ch.equals(y)) {
      ch = x; // Substitute ch by x
    }
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {
    throw new RuntimeException("Unreachable code reached at line " + lineno);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    throw new RuntimeException("Unreachable code reached at line " + lineno);
  }

  public String toStr(Env<EnvEntry> ep) throws Exception {
    return "unreachable(" + ch + ")";
  }

  public void show() {
    System.out.println(this);
  }

}
