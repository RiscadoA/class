public class ASTCellLT extends ASTType {
    private ASTType t;
    private boolean lin;
    
    public ASTCellLT(ASTType _t, boolean _lin) {
	t = _t;
	lin = _lin;
    }

    public boolean islin() {
	return lin;
    }    

    public ASTType getin() {
	return t;
    }

    public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	tc = tc.unfoldType(e);
	if(tc instanceof ASTCellLT) {
	    ASTCellLT w = (ASTCellLT)tc;
	    return (w.islin()==lin) && w.getin().equalst(t,e,lit,trail);
	}
	return false;
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }

    public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTUsageLT(t.dual(e),lin);
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	return this;
    }
    
    public String toStr(Env<EnvEntry> e) throws Exception {
	String timage = lin?"LSTATEL":"STATEL";
	    return timage+t.toStr(e);
    }

    public  ASTType subst(Env<ASTType> e) {
	return new ASTCellLT(t.subst(e), lin);
    }

    
}
