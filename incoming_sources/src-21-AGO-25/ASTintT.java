public class ASTintT extends ASTBasicType {

    public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	t = t.unfoldType(e);
	return (t instanceof ASTintT);
    }


    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
    }

    public ASTType dual(Env<EnvEntry> e) {
	return new ASTCointT();
    }

    public String toStr(Env<EnvEntry> e) {
	return "INT";
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
	return base;
    }

}
