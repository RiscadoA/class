public class ASTUsageT extends ASTType{
    
    ASTType t;
    boolean lin;
    
    public ASTUsageT(ASTType _t, boolean _lin) {
	t = _t;
	lin = _lin;
    }

    public boolean islin() {
	return lin;
    }

    public ASTType getin() {
	return t;
    }

    public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
        ty=ty.unfoldType(e);
	if(ty instanceof ASTUsageT) {
	    ASTUsageT w = (ASTUsageT)ty;
	    return (w.islin()==lin) && w.getin().equalst(t,e,lit,trail);
	}
	return false;
    }

    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }

    public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTCellT(t.dual(e),lin);
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	return this;
    }

    public String toStr(Env<EnvEntry> e) throws Exception {
	String timage = lin?"LUSAGE":"USAGE";
	    return timage+" "+t.toStr(e);
    }

    public  ASTType subst(Env<ASTType> e) {
	return new ASTUsageT(t.subst(e),lin);
    }

   public int SetOffsets(int base, Env<EnvEntry> e) throws Exception
    {
	// offset = base;
	return base;
    }    
    
}
