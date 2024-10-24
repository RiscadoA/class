package pt.inescid.cllsj;

import pt.inescid.cllsj.ast.types.ASTType;

public class TypeClosure extends SessionField {

  ASTType ty;
  Env<EnvEntry> ep;

  public TypeClosure(ASTType _ty, Env<EnvEntry> _ep) throws Exception {
    ty = _ty;
    ep = _ep;
    if (ep == null) {
      System.out.println("TypeClosure(null) " + _ty.toStr(_ep));
      System.exit(0);
    }
  }

  public ASTType getTy() {
    return ty;
  }

  public void setTy(ASTType _ty) {
    ty = _ty;
  }

  public Env<EnvEntry> getEnv() throws Exception {
    if (ep == null) {
      System.out.println("EP null?? " + ty.toStr(ep));
      System.exit(0);
    }
    return ep;
  }

  public void setEnv(Env<EnvEntry> _ep) {
    ep = _ep;
  }
}
