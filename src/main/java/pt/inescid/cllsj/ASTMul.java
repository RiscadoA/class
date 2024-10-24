package pt.inescid.cllsj;

import java.util.*;

public class ASTMul extends ASTExpr {

  ASTExpr lhs, rhs;

  public ASTMul(ASTExpr _lhs, ASTExpr _rhs) {
    lhs = _lhs;
    rhs = _rhs;
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    if (caller == lhs) lhs = (ASTExpr) newCont;
    else rhs = (ASTExpr) newCont;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {}

  public Set<String> fn(Set<String> s) {
    s = lhs.fn(s);
    s = rhs.fn(s);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s = lhs.fnLinear(s);
    s = rhs.fnLinear(s);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    lhs.subs(x, y);
    rhs.subs(x, y);
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

    ASTType lhst = lhs.etypecheck(ed, eglhs, ep, lin);

    Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());

    ASTType rhst = rhs.etypecheck(ed, egrhs, ep, lin);
    if (!(lhst instanceof ASTLCointT && rhst instanceof ASTLCointT))
      throw new TypeError("Line " + lineno + " :" + "+ : expression arguments not of COINT type");
    return new ASTLCointT();
  }

  public Value sameval(Env<SessionField> env) throws Exception {
    VInt vleft = (VInt) lhs.sameval(env);
    VInt vright = (VInt) rhs.sameval(env);
    return new VInt(vleft.get() * vright.get());
  }

  public Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception {
    VInt vleft = (VInt) lhs.eval(ed, eg);
    VInt vright = (VInt) rhs.eval(ed, eg);
    return new VInt(vleft.get() * vright.get());
  }
}
