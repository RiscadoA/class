package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.VInt;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTLCointT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTDiv extends ASTExpr {

  ASTExpr lhs, rhs;

  public ASTDiv(ASTExpr _lhs, ASTExpr _rhs) {
    lhs = _lhs;
    rhs = _rhs;
  }

  public ASTExpr getLhs() {
    return lhs;
  }

  public ASTExpr getRhs() {
    return rhs;
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

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

    ASTType lhst = lhs.etypecheck(ed, eglhs, ep, lin);

    Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());

    ASTType rhst = rhs.etypecheck(ed, egrhs, ep, lin);

    boolean lhsti = lhst instanceof ASTLCointT || lhst instanceof ASTCointT;
    boolean rhsti = rhst instanceof ASTLCointT || rhst instanceof ASTCointT;
    if (lhsti && rhsti) return new ASTCointT(); // NON-LIN-INT
    throw new TypeError("Line " + lineno + " :" + "- : expression arguments not of COINT type");
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    rhs.subs(x, y);
    lhs.subs(x, y);
  }

  public Value sameval(Env<SessionField> env) throws Exception {
    VInt vleft = (VInt) lhs.sameval(env);
    VInt vright = (VInt) rhs.sameval(env);
    return new VInt(vleft.get() / vright.get());
  }

  public Value eval(Env<Session> ed, Env<Server> eg) throws Exception {
    VInt vleft = (VInt) lhs.eval(ed, eg);
    VInt vright = (VInt) rhs.eval(ed, eg);
    return new VInt(vleft.get() / vright.get());
  }

  @Override
  public void accept(ASTExprVisitor visitor) {
    visitor.visit(this);
  }
}
