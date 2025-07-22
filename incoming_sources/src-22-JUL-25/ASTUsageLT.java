public class ASTUsageLT extends ASTType {
    
    ASTType t;
    boolean lin;
    
    public ASTUsageLT(ASTType _t, boolean _lin) {
	t = _t;
	lin = _lin;
    }

    public ASTType getin() {
	return t;
    }

    public boolean islin() {
	return lin;
    }

    public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	ty=ty.unfoldType(e);
	if(ty instanceof ASTUsageLT) {
	    ASTUsageLT w = (ASTUsageLT)ty;
	    return (w.islin()==lin) && w.getin().equalst(t,e,lit,trail);
	}
	return false;
    }

    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }

    public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTCellLT(t.dual(e),lin);
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    { 
	//	return new ASTUsageLT(t.unfoldType(e));
	return this;
    }

    public String toStr(Env<EnvEntry> e) throws Exception {
	String timage = lin?"LUSAGEL":"USAGEL";
	    return timage+t.toStr(e);
    }

        public  ASTType subst(Env<ASTType> e) {
	    return new ASTUsageLT(t.subst(e), lin);
    }

}
