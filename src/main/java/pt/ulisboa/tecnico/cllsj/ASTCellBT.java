package pt.ulisboa.tecnico.cllsj;

public class ASTCellBT extends ASTType {
  ASTType t;

  public ASTCellBT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    // System.out.println(" CELLT <-> "+tc+" \n");
    tc = tc.unfoldType(e);
    if (tc instanceof ASTCellBT) {
      ASTCellBT w = (ASTCellBT) tc;
      // System.out.println("CELLT equalst "+this+" == "+w.getin()+" "+t);
      return w.getin().equalst(t, e, lit, trail);
    }
    // else if (tc instanceof ASTIdT) {
    //    return false;
    // }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTUsageBT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
    //	ASTType ty=t.unfoldType(e);
    // return new ASTCellBT(ty);
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "CELL! " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTCellBT(t.subst(e));
  }
}
