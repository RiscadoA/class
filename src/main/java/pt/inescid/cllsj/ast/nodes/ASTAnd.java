package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.VBool;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCoLboolT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTAnd extends ASTExpr {

  ASTExpr lhs, rhs;

  public ASTAnd(ASTExpr _lhs, ASTExpr _rhs) {
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

  public void subs(String x, String y) {
    lhs.subs(x, y);
    rhs.subs(x, y);
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception {
    Env<ASTType> eglhs = eg.assoc("$DUMMY", new ASTBotT());

    ASTType lhst = lhs.etypecheck(ed, eglhs, ep, lin);

    Env<ASTType> egrhs = eg.assoc("$DUMMY", new ASTBotT());

    ASTType rhst = rhs.etypecheck(ed, egrhs, ep, lin);

    if (!(lhst instanceof ASTCoLboolT && rhst instanceof ASTCoLboolT))
      throw new TypeError("Line " + lineno + " :" + "+ : expression arguments not of LCOBOOL type");
    return new ASTCoLboolT();
  }

  public Value eval(Env<Session> ed, Env<Server> eg) throws Exception {
    VBool vleft = (VBool) lhs.eval(ed, eg);
    VBool vright = (VBool) rhs.eval(ed, eg);
    return new VBool(vleft.get() && vright.get());
  }

  public Value sameval(Env<SessionField> ed) throws Exception {
    VBool vleft = (VBool) lhs.sameval(ed);
    VBool vright = (VBool) rhs.sameval(ed);
    return new VBool(vleft.get() && vright.get());
  }

  @Override
  public void accept(ASTExprVisitor visitor) {
    visitor.visit(this);
  }
}
