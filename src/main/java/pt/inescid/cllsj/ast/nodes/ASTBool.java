package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.VBool;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.types.ASTCoLboolT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTBool extends ASTExpr {

  boolean v;

  public ASTBool(boolean _v) {
    v = _v;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {}

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public Set<String> fn(Set<String> s) {
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    return new ASTCoLboolT();
  }

  public Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception {
    return new VBool(v);
  }

  public Value sameval(Env<SessionField> ed) throws Exception {
    return new VBool(v);
  }
}
