public class ASTCoLstringT extends ASTCoLBasicType {

    public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	t = t.unfoldType(e);
	return (t instanceof ASTCoLstringT);
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
    }

    public ASTType dual(Env<EnvEntry> e) {
	return new ASTLstringT();
    }

    public String toStr(Env<EnvEntry> e) {
	return "COLSTRING";
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception
    {
	return this;
    }

    public  ASTType subst(Env<ASTType> e) {
	    return this;
    }
    
    public int SetOffsets(int base, Env<EnvEntry> e) throws Exception
    {
	// offset = base;
	return base;
    }

}
