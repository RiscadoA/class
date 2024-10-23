package pt.ulisboa.tecnico.cllsj;

import java.util.*;

public class ASTString extends ASTExpr {

  String v;

  public ASTString(String _v) {
    v = _v;
  }

  public void ASTInsertUse(String ch, ASTNode here) throws Exception {
    throw new Exception("invalud call editASTInsertUse"); // never called
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {}

  public Set<String> fn(Set<String> s) {
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    return new ASTCoLstringT();
  }

  public Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception {
    return new VString(v);
  }

  public Value sameval(Env<SessionField> env) throws Exception {
    return new VString(v);
  }
}
