import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTDList extends ASTNode {

    List<ASTTypeDef> ld;
    boolean rec;
    
    public ASTDList(List<ASTTypeDef> _lld, boolean _rec) {
	rec = _rec;
	ld = _lld;
    }

    public void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {	throw new Exception("ASTInsertPipe: call not expected");
    }

    public void ASTInsertUse(String ch, ASTType _t, ASTNode here, Boolean disCont) throws Exception
    {
	throw new Exception ("Unexpected call editASTInsertUse"); // never called
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
	throw new Exception ("Unexpected call.");
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	throw new Exception ("Unexpected call: ASTInsertCall"); // never called
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    }

    public void typecheckmany(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	if (rec  )
	    for (ASTTypeDef d: ld) {
		ep=ep.assoc(d.id,new TypeDefEntry(d));
	    }	    
	for (ASTTypeDef d: ld) {
	    d.typecheck(ed,eg,ep); 
	};
    }

    public  Env<EnvEntry> definemany(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	for (ASTTypeDef d: ld) {
	    ep=ep.assoc(d.id,new TypeDefEntry(d));
	    System.out.println("Type "+d.id+": defined.");
	};
	return ep;
    }

    
    public ASTNode subst(Env<ASTType> e) {
	return this;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
    }

    public Set<String> fn(Set<String> s) {
        return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	return s;
    }

    public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger) throws Exception{
    }

}
