package pt.ulisboa.tecnico.cllsj;

public class ASTCellT extends ASTType{
    ASTType t;
    public ASTCellT(ASTType _t) {
	t = _t;
    }


    public ASTType getin() {
	return t;
    }

    public boolean equalst(ASTType tc, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
        // System.out.println(" CELLT <-> "+tc+" \n");
	tc = tc.unfoldType(e);
	//	tc = ASTType.unfoldRec(tc);
	if(tc instanceof ASTCellT) {
	    ASTCellT w = (ASTCellT)tc;
	    // System.out.println("CELLT equalst "+this+" == "+w.getin()+" "+t);
	    return w.getin().equalst(t,e,lit,trail);
	}
	// else if (tc instanceof ASTIdT) {
	//    return false;
	// }
	return false;
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }

        public ASTType dual(Env<EnvEntry> e) throws Exception {
	return new ASTUsageT(t.dual(e));
    }

     public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	return this;
	//	ASTType ty=t.unfoldType(e);
	// return new ASTCellT(ty);
    }
    
    public String toStr(Env<EnvEntry> e) throws Exception {
	return "CELL "+t.toStr(e);
    }

    public  ASTType subst(Env<ASTType> e) {
	    return new ASTCellT(t.subst(e));
    }
    
    public int SetOffsets(int base) throws Exception
    {
	// 	offset = base;
	return base;
    }
   public int SetOffsets(int base, Env<EnvEntry> e) throws Exception
    {
	// offset = base;
	return base;
    }    
    
}
