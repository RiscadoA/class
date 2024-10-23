package pt.ulisboa.tecnico.cllsj;

public class ASTCoLBasicType extends ASTCoBasicType {

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    t = t.unfoldType(e);
    return (t instanceof ASTCoBasicType);
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {}

  public ASTType dual(Env<EnvEntry> e) {
    return null;
  }

  public String toStr(Env<EnvEntry> e) {
    return null;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public ASTType subst(Env<ASTType> e) {
    return this;
  }
}
