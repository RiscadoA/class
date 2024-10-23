package pt.ulisboa.tecnico.cllsj;

public class ASTRecvT extends ASTType {
    ASTType lhs;
    ASTType rhs;

        public ASTRecvT(ASTType _lhs,ASTType _rhs) {
	lhs = _lhs;
	rhs = _rhs;
    }

    public ASTType getlhs() {
	return lhs;
    }

    public ASTType getrhs() {
	return rhs;
    }

    public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	t = t.unfoldType(e);
	//	t = ASTType.unfoldRec(t);
	if(t instanceof ASTRecvT) {
	    ASTRecvT w = (ASTRecvT)t;
	    if (! w.getlhs().equalst(lhs,e,lit,trail) ) return false;
	    if (! w.getrhs().equalst(rhs,e,lit,trail) ) return false;
	    return true;}
	return false;
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	lhs.kindcheck(e);
        rhs.kindcheck(e);
    }

    public ASTType dual(Env<EnvEntry> e) throws Exception{
	return new ASTSendT(lhs.dual(e), rhs.dual(e));
    }


    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	/*
	ASTType lhst = lhs.unfoldType(e);
	ASTType rhst = rhs.unfoldType(e);
	return new ASTRecvT(lhst, rhst);
	*/
	return this;
    }
    

    public String toStr(Env<EnvEntry> e) throws Exception {
	return "RECV "+lhs.toStr(e)+";"+rhs.toStr(e);
    }

    public  ASTType subst(Env<ASTType> e) {
	return new ASTRecvT(lhs.subst(e), rhs.subst(e));
    }

    public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception
    {
	// offset = base;
	lhs.SetOffsets(0,ep);
	return rhs.SetOffsets(base+1,ep);
    }

}
