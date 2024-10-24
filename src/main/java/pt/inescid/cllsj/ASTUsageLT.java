package pt.inescid.cllsj;

public class ASTUsageLT extends ASTType {

  ASTType t;

  public ASTUsageLT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    ty = ty.unfoldType(e);
    if (ty instanceof ASTUsageLT) {
      ASTUsageLT w = (ASTUsageLT) ty;
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTCellLT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    //	return new ASTUsageLT(t.unfoldType(e));
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "USAGEL " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTUsageLT(t.subst(e));
  }
}
