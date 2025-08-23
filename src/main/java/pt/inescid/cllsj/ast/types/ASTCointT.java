package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;

public class ASTCointT extends ASTCoBasicType {

  public boolean equalst(ASTType t, Env<EnvEntry> e) throws Exception {
    t = t.unfoldType(e);
    return (t instanceof ASTCointT);
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {}

  public ASTType dual(Env<EnvEntry> e) {
    return new ASTintT();
  }

  public String toStr(Env<EnvEntry> e) {
    return "COINT";
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
