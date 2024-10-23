package pt.ulisboa.tecnico.cllsj;

public class ASTCoRecT extends ASTType {
  String id;
  ASTType t;
  ASTRecT dual;

  public ASTCoRecT(String _id, ASTType _t) {
    t = _t;
    id = _id;
  }

  public ASTType getin() {
    return t;
  }

  public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    ty = ty.unfoldType(e); // only to reveal structure
    // System.out.println("DEBUG equalst: ASTRECT eq "+this.toStr(e) + " == "+ ty.toStr(e));
    if (ty instanceof ASTCoRecT) {
      // System.out.println(" DEBUG equalst: ASTRECT2 eq "+t.toStr(e) + " == "+
      // ((ASTRecT)ty).getin().unfoldType(e).toStr(e));
      return t.equalst(((ASTCoRecT) ty).getin(), e, lit, trail);
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    t.kindcheck(e);
  }

  /*    public void setDual(Env<EnvEntry> e) throws Exception {
      Env<ASTType> rho = new Env<ASTType>();
      String fresh = ASTType.gensym() + "Co" + id;
      ASTType gs = new ASTIdT(fresh);
      EnvEntry gse = new TypeEntry(gs);
      e=e.assoc(fresh,gse);
      rho = rho.assoc(id, new ASTNotT(new ASTIdT(fresh)));
      dual=  new ASTRecT(fresh, t.subst(rho).dual(e));
      //CLLSj.ep=CLLSj.ep.assoc(fresh,new TypeDefEntry(td)); PR: does not work
  }*/

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    return new ASTRecT(id, t.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "Corec " + id + "." + t.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    return new ASTCoRecT(id, t.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
    // offset = base;
    return t.SetOffsets(base, ep);
  }
}
