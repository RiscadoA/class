package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
import java.util.function.*;

// unfolded

public class ASTCall extends ASTNode {
    String chr;
    String chi;
    ASTType type;
    ASTNode rhs;

    public ASTCall(String _chr,String _chi,ASTType _type,ASTNode _rhs) {
	chr = _chr;
	chi = _chi;
	type = _type;
	rhs = _rhs;
    }

    public  void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {
	if (from==rhs) { ASTNode nnode = f.apply(from);
	    rhs.setanc(nnode);
	    rhs = nnode;
	    nnode.setanc(this);
	} else {
	    throw new Exception("ASTInsertPipe: call not expected");   
	}
    }

    public  void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	if(_ch.equals(chi)) {
	    ASTUse pushUse = new ASTUse(_ch, here);
	    pushUse.setrhs(t);
	    pushUse.eg = eg;
	    here.setanc(pushUse);
	    pushUse.setanc(this);
	    rhs = pushUse;
	}else
	    anc.ASTInsertUse(_ch,t,  this, disCont);
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	ASTNode pushCall = new ASTCall(ch, cho, t,here);
	pushCall.eg = eg;
	here.setanc(pushCall);
	pushCall.setanc(this);
	rhs = pushCall;
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	if(_ch.equals(chi)) {
	    ASTNode pushWhy = new ASTWhy(_ch, here);
	    pushWhy.eg = eg;
	    here.setanc(pushWhy);
	    pushWhy.setanc(this);
	    rhs = pushWhy;
	    eg.insert(_ch, _t);
	} else
	    anc.ASTInsertWhyNot(_ch, _t, this);
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	rhs = newCont;
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception
    {
	if(_ch.equals(chi))
	    throw new TypeError("Line " + lineno + " :" +"for " + _ch + " type pending after CALL on "+chr);
	rhs = rhs.ASTweakeningOnLeaf(_ch,typ, exp);
	return this;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	this.eg = eg;

	ASTType ty;
	try {
	    ty = eg.find(chr);
	    ty = ty.unfoldType(ep);
	} catch(Exception e){
	    this.inferUses(chr,ed,ep);

	    // System.out.println("Look on ed "+chr);
	    ty = ed.find(chr);
	    // System.out.println("found on ed "+chr);
	    ty = ty.unfoldType(ep);

	    if(ty instanceof ASTWhyT){
		ASTWhyT t = (ASTWhyT) ty;
		ty = t.getin();;
		this.getanc().ASTInsertWhyNot(chr, ty, this);
		// eg.crawl();
		ed.updmove(chr);
	    }else
		throw  new TypeError("Line " + lineno + " :" +"CALL: "+chr+" is neither unrestricted nor does it type linearly with ?" +
				     "but with " + ty.toStr(ep));
	}


	// System.out.println("TC CALL unfolded ty "+ty.unfoldType(ep).toStr(ep)+ " go to unf "+type);

	ASTType typee = (type==null)?null:type.unfoldType(ep);	;
	
	// System.out.println("TC CALL equalst "+typee+" "+ty);

	if(typee!=null && !typee.equalst(ty,ep,true,new Trail()))
	    throw  new TypeError("Line " + lineno + " :" +"CALL "+chr+" type mismatch: declared="+typee.toStr(ep)+" found="+ty.toStr(ep));

	type = ty;
	
	ed=ed.assoc(chi,ty);
	
	ep = ASTNode.propagateRVar(ep, chr, chi);
	
	rhs.typecheck(ed,eg,ep);
	rhs.linclose(ed,ep);
	rhs.linclose(chi,ed,ep);


    }

    public Set<String> fn(Set<String> s) {
	s=rhs.fn(s);
	s.remove(chi);
	s.add(chr);
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	s=rhs.fnLinear(s);
	s.remove(chi);
	return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	ASTType ts = (type==null)?null:type.subst(e);
	ASTCall p = new ASTCall (chr,chi,ts,rhs.subst(e));
	p.rhs.setanc(p);
	return p;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
	if(y == chr) {
	    chr = x;
	    rhs.subs(x,y);
	}
	else if(x == chi){//we rename the bound name chi to fresh to avoid capturing name x
	    String fresh = ASTNode.gensym();
	    rhs.subs(fresh, chi);
	    chi = fresh;
	    rhs.subs(x,y);
	}
	else if(y != chi)
	    rhs.subs(x,y);
    }

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception{
	Server server = eg.find(chr);

	LinSession session = (LinSession) server.call(chi);
	logger.info("CALL server " + server.chi + " on session " + session.getId());
	rhs.runproc(ep, ed.assoc(chi, session), eg, logger);

    }
    

    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {

	SessionField val = frame.find(chr);

	if (val instanceof Value) {
		
	    if (CLLSj.trace) {
		System.out.println("call-op "+chr+" "+val);
	    }
	    
	    Env<SessionField> fnext = frame.assoc(chi,val);

	    p_cont.code = rhs;
	    p_cont.frame = fnext;
	    p_cont.epnm = ep;
	    
	    //	    rhs.sam(fnext,ep);

	} else {

	    SessionClosure repl = (SessionClosure)frame.find(chr);
	    int sessionsize = repl.getSize();
	    SessionRecord sreco =  SessionRecord.newSessionRecord(sessionsize+1);

	    if (CLLSj.trace) {
		System.out.println("call-op "+chr+" "+sreco+" "+repl+" size="+sessionsize);
	    }
	    
	    IndexedSessionRef srecfw = new IndexedSessionRef(0,sreco);
	    IndexedSessionRef srecfr = new IndexedSessionRef(0,sreco);

	    String id = repl.getId();
	    Env<SessionField> frameloc = repl.getEnv();
	    Env<EnvEntry> framep = repl.getEnvP();

	    if (!type.isPos(ep)) { // !A with A negative, continuation of call writes to id

		Env<SessionField> fwrite = frameloc.assoc(id,srecfw);
		Env<SessionField> fread = frame.assoc(chi,srecfr);
		
		sreco.setPol(true); 
		sreco.setPolDual(false); 

		sreco.setcch(chi);
		sreco.setCont(rhs);
		sreco.setFrame(fread);
		sreco.setFrameP(ep);
		
		p_cont.code = repl.getBody();
		p_cont.frame = fwrite;
		p_cont.epnm = framep;
			
	    } else {
		    
		Env<SessionField> fwrite = frame.assoc(chi,srecfw);
		Env<SessionField> fread = frameloc.assoc(id,srecfr);

		sreco.setPol(true); 
		sreco.setPolDual(false);
		    
		sreco.setcch(id);
		sreco.setCont(repl.getBody());
		sreco.setFrame(fread);
		sreco.setFrameP(framep);

		p_cont.code = rhs;
		p_cont.frame = fwrite;
		p_cont.epnm = ep;

	    }

	}


    }
    
    public void show()  {
	System.out.println(this+" "+chr+"("+chi+")");
	rhs.show();
    }

    
}
