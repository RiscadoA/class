package pt.inescid.cllsj.ast.nodes;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;

public class ASTNotT extends ASTType {
  ASTType t;

  public ASTNotT(ASTType _t) {
    t = _t;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType tt, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    // System.out.println("DEBUG equalst ASTNotT: " + lit+" EQST "+this.toStr(e) + " == " +
    // tt.toStr(e));
    if (!lit) {
      if (!(tt instanceof ASTNotT)) return false;
      ASTType tn = ((ASTNotT) tt).getin();
      return tn.equalst(t, e, lit, trail);
    } else {
      if (tt instanceof ASTIdT) {
        // System.out.println("HEREX "+tt);

        return tt.equalst(this, e, true, trail);
      } else {
        ASTType tn = tt.dual(e);
        return tn.equalst(t, e, lit, trail);
      }
    }
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    // System.out.println("+DUAL NOT  >> "+t);
    return t;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    ASTType ty = t.unfoldType(e);
    // System.out.println("DUAL NOT  >> "+t+" "+ty);
    ty = ty.dual(e);
    return ty;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "~" + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTNotT(t.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
    return base;
  }
}
