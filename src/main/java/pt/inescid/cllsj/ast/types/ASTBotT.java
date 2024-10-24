package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;

public class ASTBotT extends ASTType {

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) {
    return (t instanceof ASTBotT);
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {}

  public ASTType dual(Env<EnvEntry> e) {
    return new ASTOneT();
  }

  public String toStr(Env<EnvEntry> e) {
    return "CCLOSE";
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public ASTType subst(Env<ASTType> e) {
    return this;
  }

  public int SetOffsets(int base, Env<EnvEntry> e) throws Exception {
    // offset = base;
    return base;
  }
}
