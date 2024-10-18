import java.util.*;

public class ASTInt extends ASTExpr {

    int v;

    public ASTInt(int _v) {
	v = _v;
    }

     public void ASTInsertUse(String ch, ASTNode here, Boolean disCont) throws Exception
    {
	 throw new Exception ("Unexpected call editASTInsertUse"); // never called
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
        throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
        throw new Exception ("Unexpected call.");
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
        throw new Exception ("Unexpected call: ASTInsertCall"); // never called
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    }

    public Set<String> fn(Set<String> s) {
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
        return s;
    }

    public ASTNode subst(Env<ASTType> e) {
		return this;
	}

    public void subs(String x, String y){// implements x/y (substitutes y by x)
    }

    public ASTType etypecheck(Env<ASTType> ed, Env<ASTType> eg,Env<EnvEntry> ep, boolean lin) throws Exception {
	return new ASTLCointT();
    }

    public Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception
    {
        return new VInt(v);
    }


    public  Value sameval(Env<SessionField> ed) throws Exception
     {
        return new VInt(v);
    }

}
