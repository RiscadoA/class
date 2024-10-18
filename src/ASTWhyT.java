public class ASTWhyT extends ASTType{
    ASTType t;
    public ASTWhyT(ASTType _t) {
	t = _t;
    }

    public ASTType getin() {
	return t;
    }

    public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	ty = ty.unfoldType(e);
	if(ty instanceof ASTWhyT) {
	    ASTWhyT w = (ASTWhyT)ty;
	    return w.getin().unfoldType(e).equalst(t.unfoldType(e),e,lit,trail);
	}
	return false;
    }

    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }

    
    public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTBangT(t.dual(e));
    }

     public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	return this;
    }
   

        public String toStr(Env<EnvEntry> e) throws Exception {
	    return "?"+t.toStr(e);
    }


    public  ASTType subst(Env<ASTType> e) {
	return new ASTWhyT(t.subst(e));
    }

   public int SetOffsets(int base, Env<EnvEntry> e) throws Exception
    {
	// offset = base;
	return base+1;
    }    
    

}
