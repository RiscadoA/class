package pt.ulisboa.tecnico.cllsj;

public class ASTRecT extends ASTType {
   String id;
   ASTType t;
   ASTCoRecT dual;
    
    public ASTRecT(String _id, ASTType _t) {
	t = _t;
       id = _id;
    }

    public ASTType getin() {
	return t;
    }

    /** MUST CHANGE TO ISO-RECURSIVE, because of SAM and compilation
    */
    /*
    public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	ty = ty.unfoldType(e); // only to reveal structure
	 //System.out.println("DEBUG equalst: ASTRECT eq "+this.toStr(e) + " == "+ ty.toStr(e));
		if(ty instanceof ASTRecT) {
		    //System.out.println(" DEBUG equalst: ASTRECT2 eq "+t.toStr(e) + " == "+ ((ASTRecT)ty).getin().unfoldType(e).toStr(e));
		    return t.equalst(((ASTRecT)ty).getin().unfoldType(e),e,lit,trail);
	    	}
		return false;
		}
    */
    
    public boolean equalst(ASTType ty, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	ty = ty.unfoldType(e); // only to reveal structure
	// System.out.println(" DEBUG equalst: ASTREC eq "+ty.toStr(e));
	if(ty instanceof ASTRecT) {
	    // System.out.println(" DEBUG equalst: ASTRECT2 eq "+t.toStr(e) + " == "+ ((ASTRecT)ty).getin().toStr(e));
	    return t.equalst(((ASTRecT)ty).getin(),e,lit,trail);
	}
	return false;
    }

    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	t.kindcheck(e);
    }
    
    public ASTType dual(Env<EnvEntry> e) throws Exception {	
        return new ASTCoRecT(id, t.dual(e));
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	return this;
    }
   

    public String toStr(Env<EnvEntry> e) throws Exception {
	return "Rec "+id+"."+t.toStr(e);
    }

    public  ASTType subst(Env<ASTType> e) {
	return new ASTRecT(id,t.subst(e));
    }

    public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception
    {
	// offset = base;
	return t. SetOffsets(base+1,ep);
    }
    
}
