public class ASTBangT extends ASTType {
    ASTType t;
    
    public ASTBangT(ASTType _t) {
	t = _t;
    }


    public ASTType getin() {
	return t;
    }

    public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	ty = ty.unfoldType(e);
	// t = ASTType.unfoldRec(t);
	if(ty instanceof ASTBangT) {
	    ASTBangT w = (ASTBangT)ty;
	    return w.getin().equalst(t,e,lit,trail);
	}
	return false;
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }


    public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTWhyT(t.dual(e));
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	// ASTType ty=t.unfoldType(e);
	// return new ASTBangT(ty);
	return this;
    }


    public String toStr(Env<EnvEntry> e) throws Exception {
	//System.out.println("BANG toStr "+t);
	return "!"+t.toStr(e);
    }
    
    public  ASTType subst(Env<ASTType> e) {
	return new ASTBangT(t.subst(e));
    }

    
   public int SetOffsets(int base, Env<EnvEntry> e) throws Exception
    {
	// offset = base;
	return base+1;
    }    
}
