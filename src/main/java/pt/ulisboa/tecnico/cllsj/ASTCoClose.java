package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
import java.util.function.*;

// unfolded

public class ASTCoClose extends ASTNode{
    String ch;
    ASTNode rhs;


    public ASTCoClose(String _ch, ASTNode _rhs) {
	ch = _ch;
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
	anc.ASTInsertUse(_ch, t, this, disCont);
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	ASTNode pushCall = new ASTCall(ch, cho, t,here);
	pushCall.eg = eg;
	here.setanc(pushCall);
	pushCall.setanc(this);
	rhs = pushCall;
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
    	rhs = newCont;
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception
    {
	rhs = rhs.ASTweakeningOnLeaf(_ch,typ, exp);
	// System.out.println("coclose rhs "+rhs);
	return this;
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	anc.ASTInsertWhyNot(_ch, _t, this);
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	this.eg = eg;
	// this.inferUses(ch,ed,ep);

	ASTType ty = ed.find(ch);
	ty = ty.unfoldType(ep);
	ty = ASTType.unfoldRecInfer(ty, this, ch, ep);
	
	if (ty instanceof ASTBotT) {
				
	    ed.upd(ch,null); 
	    rhs.typecheck(ed,eg,ep);
	} else throw new TypeError("Line " + lineno + " :" +
				   "WAIT: "+ch+" is not of WAIT type, found: "+ty.toStr(ep));
    }

    public Set<String> fn(Set<String> s) {
	s.add(ch);
	s = rhs.fn(s);
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	s.add(ch);
	s = rhs.fnLinear(s);
	return s;
    }

    public ASTNode subst(Env<ASTType> e) {	    
	ASTCoClose p = new ASTCoClose(ch,rhs.subst(e));
	p.rhs.setanc(p);
	return p;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
	if(y == ch)
	    ch = x;
	else
	    rhs.subs(x,y);
    }

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception{
	//CLLSj.logger.info("COCLOSE starts.");
	Channel channel = (Channel) ed.find(ch);
	//CLLSj.logger.info("COCLOSE: found channel. ");
	//System.out.println("[RunStatus] WAIT on "+ch+" start.");
	//System.out.println("[RunStatus] pre WAIT on "+channel.getId()+" end.");
	channel.receive();
	//System.out.println("[RunStatus] post WAIT on "+channel.getId()+" end.");
	rhs.runproc(ep, ed, eg, logger);
    }

    public void show()  {
	System.out.println(this);
	rhs.show();
    }

    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	SessionField srefi = frame.find(ch); 
	if (srefi instanceof LinSessionValue)  { // concurrent channel
	    // System.out.println("wait-op "+srefi+" "+((LinSessionValue)srefi).getLin());
	    ((Channel)(((LinSessionValue)srefi).getLin())).receive();
	    // System.out.println("after receive");
	    if (CLLSj.trace) {
		System.out.println("cwait-op "+ch);
	    }	    
	    p_cont.code = rhs;
	    // System.out.println("wait-op cont  "+ rhs);
	    p_cont.frame = frame;
	    p_cont.epnm = ep;
	} else 
	    { 
		IndexedSessionRef sref = (IndexedSessionRef)srefi;
		int doffset = sref.getOffset();
		SessionRecord srec = sref.getSessionRec();
		boolean pol = srec.getPol();
		if(pol) {
		    if (CLLSj.trace) {
			System.out.println("wait-op S [-] "+ srec.getcch());
		    }
		    int i = 8/0; // never happens
		    ASTNode cont = srec.getCont();
		    Env<SessionField> frm  = srec.getFrame();
		    Env<EnvEntry> epn  = srec.getFrameP();
		    boolean pold = srec.getPolDual();
		
		    srec.setPolDual(srec.getPol());   // this session dies so don't really care
		    srec.setPol(pold); 

		    srec.setCont(this); // may be this should be GC'ed ?
		    srec.setcch(ch);
		    srec.setFrame(frame);
		    srec.setFrameP(ep);
	    
		    p_cont.code = cont;
		    p_cont.frame = frm;
		    p_cont.epnm = epn;
		    return;
		} else {

		    if (CLLSj.trace) {
			System.out.println("wait-op "+ch+" "+srec+" @ "+doffset);
		    }
		    if (srec.readSlot(doffset) == null)
			throw new SAMError("SAM-WAIT-read-FAILURE "+ch);
		    srec.writeSlot(null,doffset);
		    sref.incOffset();
	    
		    srec.setPol(rhs.isPos()); // this session dies so don't really care
	    
		    p_cont.code = rhs;
		    frame.upd(ch,null); // deletefromenv -- seems OK //
		    p_cont.frame = frame; 
		    p_cont.epnm = ep;
		    SessionRecord.freeSessionRecord(srec);   	
		}
	    }
    }

    
}
