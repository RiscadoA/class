package pt.inescid.cllsj.ast.nodes;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;

public class ASTSendT extends ASTType {

  ASTType lhs;
  ASTType rhs;

  public ASTSendT(ASTType _lhs, ASTType _rhs) {
    lhs = _lhs;
    rhs = _rhs;
  }

  public ASTType getlhs() {
    return lhs;
  }

  public ASTType getrhs() {
    return rhs;
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    t = t.unfoldType(e);
    // t = ASTType.unfoldRec(t);
    if (t instanceof ASTSendT) {
      ASTSendT w = (ASTSendT) t;
      if (!w.getlhs().equalst(lhs, e, lit, trail)) return false;
      if (!w.getrhs().equalst(rhs, e, lit, trail)) return false;
      return true;
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    lhs.kindcheck(e);
    rhs.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTRecvT(lhs.dual(e), rhs.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    // return new ASTSendT(lhs.unfoldType(e),rhs.unfoldType(e));
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "SEND " + lhs.toStr(e) + ";" + rhs.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTSendT(lhs.subst(e), rhs.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception // need to unfold
      {
    // offset = base;
    // lhs = lhs.unfoldType(ep);
    lhs.SetOffsets(0, ep);
    // rhs = rhs.unfoldType(ep);
    return rhs.SetOffsets(base + 1, ep);
  }
}
