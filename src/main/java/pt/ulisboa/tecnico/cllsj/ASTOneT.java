package pt.ulisboa.tecnico.cllsj;

public class ASTOneT extends ASTType {

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) {
    return (t instanceof ASTOneT);
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {}

  public ASTType dual(Env<EnvEntry> e) {
    return new ASTBotT();
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public String toStr(Env<EnvEntry> e) {
    return "CLOSE";
  }

  public ASTType subst(Env<ASTType> e) {
    return this;
  }

  public int SetOffsets(int base, Env<EnvEntry> e) throws Exception {
    // offset = base;
    return base;
  }
}
