public class ASTCellLT extends ASTType{
    private ASTType t;
    public ASTCellLT(ASTType _t) {
	t = _t;
    }


    public ASTType getin() {
	return t;
    }

    public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	tc = tc.unfoldType(e);
	if(tc instanceof ASTCellLT) {
	    ASTCellLT w = (ASTCellLT)tc;
	    return w.getin().equalst(t,e,lit,trail);
	}
	return false;
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }

        public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTUsageLT(t.dual(e));
    }

     public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	// ASTType ty=t.unfoldType(e);
	// return new ASTCellLT(ty);
	return this;
    }
    
    public String toStr(Env<EnvEntry> e) throws Exception {
	return "STATEL "+t.toStr(e);
    }

    public  ASTType subst(Env<ASTType> e) {
	return new ASTCellLT(t.subst(e));
    }

    
}
