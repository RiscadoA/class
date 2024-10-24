package pt.inescid.cllsj;

public class ASTCoAffineT extends ASTType {
  ASTType t;

  public ASTCoAffineT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    tc = tc.unfoldType(e);
    // tc = ASTType.unfoldRec(tc);
    if (tc instanceof ASTCoAffineT) {
      ASTCoAffineT w = (ASTCoAffineT) tc;
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTAffineT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    /*       ASTType ty=t.unfoldType(e);
           return new ASTCoAffineT(ty);
    */
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "CoAffine " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTCoAffineT(t.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
    // offset = base;
    return t.SetOffsets(base + 1, ep);
  }
}
