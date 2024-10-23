package pt.ulisboa.tecnico.cllsj;

import java.util.*;

public class ASTAdd extends ASTExpr {

  ASTExpr lhs, rhs;

  public ASTAdd(ASTExpr _lhs, ASTExpr _rhs) {
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

  public void subs(String x, String y) {
    lhs.subs(x, y);
    rhs.subs(x, y);
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());
    ASTType lhst = lhs.etypecheck(ed, eglhs, ep, lin);
    Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());
    ASTType rhst = rhs.etypecheck(ed, egrhs, ep, lin);

    if (lhst instanceof ASTLCointT && rhst instanceof ASTLCointT) return new ASTLCointT();

    if (lhst instanceof ASTCoLstringT || rhst instanceof ASTCoLstringT) return new ASTCoLstringT();

    throw new TypeError("Line " + lineno + " :" + "+ : illegal argument types");
  }

  Value val(Value vleftx, Value vrightx) throws Exception {
    if (vleftx instanceof VInt && vrightx instanceof VInt) {
      VInt vleft = (VInt) vleftx;
      VInt vright = (VInt) vrightx;
      return new VInt(vleft.get() + vright.get());
    } else if (vleftx instanceof VString) {
      VString vleft = (VString) vleftx;
      return new VString(vleft.get() + vrightx.toStr());
    } else if (vrightx instanceof VString) {
      VString vright = (VString) vrightx;
      return new VString(vleftx.toStr() + vright.get());
    }
    System.out.println("unreachable");
    return null; // unreachable
  }

  public Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception {
    Value vleftx = lhs.eval(ed, eg);
    Value vrightx = rhs.eval(ed, eg);
    return val(vleftx, vrightx);
  }

  public Value sameval(Env<SessionField> env) throws Exception {
    Value vleftx = lhs.sameval(env);
    Value vrightx = rhs.sameval(env);
    return val(vleftx, vrightx);
  }
}
