package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.function.*;

public class ASTInclude extends ASTNode {

    String filename;

    public ASTInclude (String _filename) {
	filename = _filename;
    }

    public void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {	throw new Exception("ASTInsertPipe: call not expected");
    }

    public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	 throw new Exception ("Unexpected call editASTInsertUse"); // never called
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
        throw new Exception ("Unexpected call: ASTInsertCall"); // never called
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
        throw new Exception ("Unexpected call.");
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
        throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception
    {
		throw new Exception ("Unexpected call: ASTweakeningOnLeaf"); // never called
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep)	throws Exception {
    }

    public Set<String> fn(Set<String> s) {
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
        return s;
    }


    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg)throws Exception{

    }

        public ASTNode subst(Env<ASTType> e) {
	    return this;
	}

    public void subs(String x, String y){// implements x/y (substitutes y by x)
    }
    public String getFn() {
	return filename.substring(1,filename.length()-1);
    }
    

}
