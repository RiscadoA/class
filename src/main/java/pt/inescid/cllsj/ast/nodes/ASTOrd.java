package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.VInt;
import pt.inescid.cllsj.VString;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.types.ASTCoLstringT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTOrd extends ASTExpr {

  ASTExpr ex;

  public ASTOrd(ASTExpr _ex) {
    ex = _ex;
  }

  public ASTExpr getExpr() {
    return ex;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {}

  public Set<String> fn(Set<String> s) {
    s = ex.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = ex.fnLinear(s);
    return s;
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    ex = (ASTExpr) newCont;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    ex.subs(x, y);
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    ASTType ext = ex.etypecheck(ed, eg, ep, lin);

    if (!(ext instanceof ASTCoLstringT))
      throw new TypeError(
          "Line " + lineno + " :" + "+ : expression argument not of COLstringL type");
    return new ASTCointT();
  }

  public Value eval(Env<Session> ed, Env<Server> eg) throws Exception {
    VString v = (VString) ex.eval(ed, eg);
    return new VInt(v.get().charAt(0));
  }

  public Value sameval(Env<SessionField> ed) throws Exception {
    VString v = (VString) ex.sameval(ed);
    return new VInt(v.get().charAt(0));
  }

  @Override
  public void accept(ASTExprVisitor visitor) {
    visitor.visit(this);
  }
}
