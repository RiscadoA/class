package pt.inescid.cllsj.ast.nodes;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;

public class ASTBasicType extends ASTType {

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    t = t.unfoldType(e);
    return (t instanceof ASTBasicType);
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    throw new TypeError("ASTBasicType Unexpected");
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    throw new TypeError("ASTBasicType Unexpected");
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    throw new TypeError("ASTBasicType Unexpected");
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public ASTType subst(Env<ASTType> e) {
    return this;
  }

  public ASTBasicType lift() throws Exception {
    if (this instanceof ASTintT) {
      return new ASTLintT();
    } else throw new TypeError("ASTBasicType Unexpected"); // unreachable
  }

  /*
  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
  return base+1;
     }
     */
}
