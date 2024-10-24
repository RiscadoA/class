package pt.inescid.cllsj;

public class ASTAffineT extends ASTType {
  ASTType t;

  public ASTAffineT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    tc = tc.unfoldType(e);
    //	tc = ASTType.unfoldRec(tc);
    if (tc instanceof ASTAffineT) {
      ASTAffineT w = (ASTAffineT) tc;
      // System.out.println("AFFINE DEBUG equalst "+w.getin()+ " "+t);
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTCoAffineT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    // ASTType ty=t.unfoldType(e);
    // return new ASTAffineT(ty);
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "Affine " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTAffineT(t.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
    // offset = base;
    return t.SetOffsets(base + 1, ep);
  }
}
