package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.VBool;
import pt.inescid.cllsj.VInt;
import pt.inescid.cllsj.VString;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCoLboolT;
import pt.inescid.cllsj.ast.types.ASTCoLstringT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTLCointT;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTLeq extends ASTExpr {

  ASTExpr lhs, rhs;

  public ASTLeq(ASTExpr _lhs, ASTExpr _rhs) {
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

    boolean lhsInt = lhst instanceof ASTLCointT || lhst instanceof ASTCointT;
    boolean rhsInt = rhst instanceof ASTLCointT || rhst instanceof ASTCointT;

    if (!((lhsInt && rhsInt)
        || (lhst instanceof ASTCoLstringT && rhst instanceof ASTCoLstringT)
        || (lhst instanceof ASTCoLboolT && rhst instanceof ASTCoLboolT)))
      throw new TypeError(
          "Line " + lineno + " :" + "== : expression arguments not of the same co-type");
    return new ASTCoLboolT();
  }

  public Value eval(Env<Session> ed, Env<Server> eg) throws Exception {
    Value vleft = lhs.eval(ed, eg);
    Value vright = rhs.eval(ed, eg);
    if (vleft instanceof VInt) {
      return new VBool(((VInt) vleft).get() <= ((VInt) vright).get());
    } else if (vleft instanceof VString) {
      return new VBool(((VString) vleft).get().compareTo(((VString) vright).get()) <= 0);
    } else throw new TypeError("< : unexpected types");
  }

  public Value sameval(Env<SessionField> ed) throws Exception {
    Value vleft = lhs.sameval(ed);
    Value vright = rhs.sameval(ed);
    if (vleft instanceof VInt) {
      return new VBool(((VInt) vleft).get() <= ((VInt) vright).get());
    } else if (vleft instanceof VString) {
      return new VBool(((VString) vleft).get().compareTo(((VString) vright).get()) <= 0);
    } else throw new TypeError("<= : unexpected types");
  }

  @Override
  public void accept(ASTExprVisitor visitor) {
    visitor.visit(this);
  }
}
