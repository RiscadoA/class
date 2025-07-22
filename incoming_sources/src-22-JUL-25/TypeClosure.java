public class TypeClosure extends SessionField {

    ASTType ty;
    Env<EnvEntry> ep;


   TypeClosure(ASTType _ty,
	      Env<EnvEntry> _ep) throws Exception
    {
	ty = _ty;
	ep = _ep;
	if (ep==null) {
	    	System.out.println("TypeClosure(null) "+_ty.toStr(_ep));
	    System.exit(0);
	}
    }

    ASTType getTy() {
	return ty;
    }
    
    void setTy(ASTType _ty) {
	 ty = _ty;
    }

    Env<EnvEntry> getEnv() throws Exception {
	if (ep==null) {
	    System.out.println("EP null?? "+ty.toStr(ep));
	    System.exit(0);
	}
	return ep;
    }
    
    void setEnv(Env<EnvEntry> _ep) {
	 ep = _ep;
    }
    
}
