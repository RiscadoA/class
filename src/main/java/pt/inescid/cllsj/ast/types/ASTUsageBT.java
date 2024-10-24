package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;

public class ASTUsageBT extends ASTType {

  ASTType t;

  public ASTUsageBT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    ty = ty.unfoldType(e);
    if (ty instanceof ASTUsageBT) {
      ASTUsageBT w = (ASTUsageBT) ty;
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTCellBT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    //	return new ASTUsageBT(t.unfoldType(e));
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "USAGE! " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTUsageBT(t.subst(e));
  }
}
