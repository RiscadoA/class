package pt.inescid.cllsj.ast.nodes;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;

public class ASTUsageT extends ASTType {

  ASTType t;

  public ASTUsageT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    // System.out.println("DEBUG equalst ASTUsageT: before unfold:  "+this.toStr(e) + " == "+
    // ty.toStr(e));
    ty = ty.unfoldType(e);
    // ty = ASTType.unfoldRec(ty);
    if (ty instanceof ASTUsageT) {
      ASTUsageT w = (ASTUsageT) ty;
      return w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTCellT(t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    //	return new ASTUsageT(t.unfoldType(e));
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "USAGE " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTUsageT(t.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> e) throws Exception {
    // offset = base;
    return base;
  }
}
