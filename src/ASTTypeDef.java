import java.util.*;
import java.util.function.*;

public class ASTTypeDef extends ASTNode {
    String id;
    ASTType rhs;
    ASTType dualcached;
    ASTTypeDef dual;
    List<String> args;
    boolean rec;
    boolean corec;
    
    public ASTTypeDef (String _id) {
	id = _id;
	args = new ArrayList<String>();
    }
    
    public void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {	throw new Exception("ASTInsertPipe: call not expected");
    }

    public void ASTInsertUse(String ch, ASTType _t, ASTNode here, Boolean disCont) throws Exception
    {
	throw new Exception ("Unexpected call editASTInsertUse"); // never called
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	throw new Exception ("Unexpected call.");
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception
    {
	throw new Exception ("Unexpected call editASTInsertCall"); // never called
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public String getid() {
	return id;
    }

    public void addPar(String pid) throws Exception {
	if(args.contains(pid)) 
	    throw new SyntaxError("Parameter "+pid+" declared twice in "+id);
	args.add(pid);
    }

    public void setType (ASTType _rhs) {
	rhs = _rhs;
    }

    public ASTType getType () {
	return rhs;
    }

    public void setDual (ASTType _d) {
	dualcached = _d;
    }

    public ASTType getDual () {
	return dualcached;
    }

    public void setRec(boolean _rec){
	rec = _rec;
    }
    
    public void setCoRec(boolean _corec){
	corec = _corec;
    }

    public boolean getRec(){
	return rec;
    }
    
    public boolean getCoRec(){
	return corec;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    	List<String> newargs = new ArrayList<String>();

	for (String param : args) {

	    String newparam = ASTType.gensym();
	    ep = ep.assoc(newparam, new TypeEntry (new ASTIdT(newparam)));

	    { Env<ASTType> e = new Env<ASTType>().assoc(param, new ASTIdT(newparam));

		rhs = rhs.subst(e);
	    }
	    newargs.add(newparam);
	}
	
	args = newargs;

	if (rec) {
	    rhs = new ASTRecT(id, rhs);
	    // System.out.println("TYPE REC "+id+" "+rhs);
	}
	if (corec) {
	    rhs = new ASTCoRecT(id, rhs);
	    // System.out.println("TYPE COREC "+id+" "+rhs);
	}

      	if (rec || corec) {
	    ep=ep.assoc(id,new TypeDefEntry(this)); 

	}
	rhs.kindcheck(ep);
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

    public void subs(String x, String y){
    }
}
