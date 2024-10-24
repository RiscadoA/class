package pt.inescid.cllsj;

public class ASTCellBLT extends ASTType {
  private ASTType t;

  public ASTCellBLT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    tc = tc.unfoldType(e);
    if (tc instanceof ASTCellBLT) {
      ASTCellBLT w = (ASTCellBLT) tc;
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTUsageBLT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    // ASTType ty=t.unfoldType(e);
    // return new ASTCellBLT(ty);
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "STATE!L " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTCellBLT(t.subst(e));
  }

  public int SetOffsets(int base) throws Exception {
    // offset = base;
    return base;
  }
}
