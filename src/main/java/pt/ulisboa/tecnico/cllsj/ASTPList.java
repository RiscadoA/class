package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTPList extends ASTNode {

    List<ASTProcDef> ld;
    boolean unsafe_rec;
    boolean rec;

    public ASTPList(List<ASTProcDef> _lld, boolean _rec, boolean _unsafe_rec) {
	rec = _rec;
	unsafe_rec = _unsafe_rec;
	ld = _lld;
    }

    public void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {	throw new Exception("ASTInsertPipe: call not expected");
    }


    public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	throw new Exception ("Unexpected call editASTInsertUse"); // never called
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
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
	throw new Exception ("Unexpected call: ASTweakeningOnLeaf"); // never called
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    }

    public void typecheckmany(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
       	if (rec || unsafe_rec)
	    for (ASTProcDef d: ld) {
		ep=ep.assoc(d.id,new ProcEntry(d));
	    }	    
	for (ASTProcDef d: ld) {
	    d.setEnv(ep);
	    d.typecheck(ed,eg,ep); 
	};
    }

    public  Env<EnvEntry> definemany(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep, boolean unsafe) throws Exception {
	for (ASTProcDef d: ld) {
	    ProcEntry pee =    new ProcEntry(d);
	    ep=ep.assoc(d.id,pee);
	    // d.setEnv(ep); 
	    if(!unsafe) System.out.println("Process "+d.id+": defined.");
	    // d.getEnv().crawl();
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


    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception{
    }

}
