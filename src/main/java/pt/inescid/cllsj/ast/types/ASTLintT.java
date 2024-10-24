package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;

public class ASTLintT extends ASTBasicType {

  public boolean equalst(ASTType t, Env<EnvEntry> e) throws Exception {
    t = t.unfoldType(e);
    return (t instanceof ASTLintT);
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {}

  public ASTType dual(Env<EnvEntry> e) {
    return new ASTLCointT();
  }

  public String toStr(Env<EnvEntry> e) {
    return "LINT";
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public ASTType subst(Env<ASTType> e) {
    return this;
  }

  public int SetOffsets(int base, Env<EnvEntry> e) throws Exception {
    return base;
  }
}
