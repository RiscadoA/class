package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.ast.ASTTypeVisitor;

public class ASTCellT extends ASTType {

  public ASTType t;
  boolean lin;

  public ASTCellT(ASTType _t, boolean _lin) {
    t = _t;
    lin = _lin;
  }

  public boolean islin() {
    return lin;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    tc = tc.unfoldType(e);
    if (tc instanceof ASTCellT) {
      ASTCellT w = (ASTCellT) tc;
      return (w.islin() == lin) && w.getin().equalst(t, e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTUsageT(t.dual(e), lin);
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
    //	ASTType ty=t.unfoldType(e);
    // return new ASTCellT(ty);
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    String timage = lin ? "LSTATE" : "STATE";
    return timage + " " + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTCellT(t.subst(e), lin);
  }

  public int SetOffsets(int base) throws Exception {
    // 	offset = base;
    return base;
  }

  public int SetOffsets(int base, Env<EnvEntry> e) throws Exception {
    // offset = base;
    return base;
  }

  @Override
  public void accept(ASTTypeVisitor visitor) {
    visitor.visit(this);
  }
}
