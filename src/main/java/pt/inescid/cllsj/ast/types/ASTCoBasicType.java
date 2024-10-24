package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;

public class ASTCoBasicType extends ASTType {

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

  public ASTCoLBasicType lift() throws Exception {

    if (this instanceof ASTCointT) {
      return new ASTLCointT();
    } else throw new TypeError("lift illegal"); // unreachable
  }
}
