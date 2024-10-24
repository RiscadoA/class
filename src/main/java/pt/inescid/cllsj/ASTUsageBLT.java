package pt.inescid.cllsj;

public class ASTUsageBLT extends ASTType {

  ASTType t;

  public ASTUsageBLT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    ty = ty.unfoldType(e);
    if (ty instanceof ASTUsageBLT) {
      ASTUsageBLT w = (ASTUsageBLT) ty;
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTCellBLT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    //	return new ASTUsageBLT(t.unfoldType(e));
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "USAGE!L " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTUsageBLT(t.subst(e));
  }
}
