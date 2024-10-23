package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.logging.*;
import java.util.function.*;

// unfolded

public class ASTUnfold extends ASTNode{
    String ch;
    ASTNode rhs;
    boolean rec;
    ASTType tyrhs;
    
    public ASTUnfold(String _ch, ASTNode _rhs) {
	ch = _ch;
	rhs = _rhs;
    }

    public void setrhs(ASTType r) {
	tyrhs = r;
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
	if(_ch.equals(ch)) {
	    ASTUse pushUse = new ASTUse(_ch, here);
	    pushUse.setrhs(t);
	    pushUse.eg = eg;
	    here.setanc(pushUse);
	    pushUse.setanc(this);
	    rhs = pushUse;
	}else
	    anc.ASTInsertUse(_ch,t, this, disCont);
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	ASTNode pushCall = new ASTCall(ch, cho, t,here);
	pushCall.eg = eg;
	here.setanc(pushCall);
	pushCall.setanc(this);
	rhs = pushCall;
    }
    
    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception
    {
	rhs = rhs.ASTweakeningOnLeaf(_ch,typ, exp);
	return this;
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	if(_ch.equals(ch)) {
	    ASTNode pushWhy = new ASTWhy(_ch, here);
	    here.setanc(pushWhy);
	    pushWhy.setanc(this);
	    rhs = pushWhy;
	    eg.insert(_ch,_t);
	} else
	    anc.ASTInsertWhyNot(_ch, _t, this);
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	rhs = newCont;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	this.eg = eg;

	ASTType ty = ed.find(ch);
	ty = ty.unfoldType(ep);
	if(ty instanceof ASTCoAffineT){
	    ASTCoAffineT tyco = (ASTCoAffineT)ty;
	    ed.upd(ch, tyco.getin());

	    ASTType cont = tyco.getin().unfoldType(ep);
	    cont = ASTType.unfoldRec(cont);
	    Boolean disposableCont = (cont instanceof ASTUsageT) || (cont instanceof ASTCoAffineT)
		|| (cont instanceof ASTWhyT);

	    this.getanc().ASTInsertUse(ch,tyco.getin(), this, disposableCont);
	    this.typecheck(ed,eg,ep);
	}
	if (ty instanceof ASTRecT)  {
	    rec = true;
	    tyrhs = ((ASTRecT)ty).getin();
	    ed.upd(ch,tyrhs); 
	    rhs.typecheck(ed,eg,ep);
	} else if (ty instanceof ASTCoRecT)  {
	    rec = false;
	    tyrhs = ((ASTCoRecT)ty).getin();
	    ed.upd(ch,tyrhs); 
	    rhs.typecheck(ed,eg,ep);
	} else throw new TypeError("Line " + lineno + " :" +"UNFOLD: "+ch+" is not of REC or COREC type " + ty);
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
	ASTUnfold p =  new ASTUnfold(ch,rhs.subst(e));
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
	rhs.runproc(ep, ed, eg, logger);
    }

    public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception
    {
	IndexedSessionRef sref = (IndexedSessionRef)frame.find(ch);
	int doffset = sref.getOffset();
	SessionRecord srec = sref.getSessionRec();
	    
	ASTNode cont = srec.getCont();
	Env<SessionField> frm  = srec.getFrame();
	Env<EnvEntry> epn  = srec.getFrameP();

	if ( false )  { // srec.getStep()) {
	    // System.out.println("unfold switch to corec "+ch+" "+srec);
	    srec.setCont(this);
	    srec.setPol(false); // !
	    srec.setcch(ch);
	    srec.setFrame(frame);
	    srec.setFrameP(ep);
	    cont.sam(frm,epn); // go for corec (or call to recursive procdef)
	} else {
	    // System.out.println("unfold -> produce " + ch + " " + srec);
	    // srec.setStep(true); 
	    sref.setOffset(0);
	    rhs.sam(frame,ep);
	}
    }

    public void samLC(String id, Channel channel, Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	if (rec) { // REC
	    if (CLLSj.trace) {
		System.out.println("unfold-op-send-c+  "+ id+ " "+rec);
	    }
	    channel.send(new SessionFieldUnfold());
	    if (CLLSj.trace) {
		System.out.println("unfold-op-send-c-  "+ id+ " "+rec);
	    }
	} else {
	    if (CLLSj.trace) {
		System.out.println("unfold-op-recv-c+  "+ id+ " "+rec);
	    }
	    SessionFieldUnfold uf = (SessionFieldUnfold)channel.receive();		
	    if (CLLSj.trace) {
		System.out.println("unfold-op-recv-c-  "+ id+ " "+rec);
	    }
	}
	
    } 
    
    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	
	SessionField sf = frame.find(ch);
	
	
	if (sf instanceof LinSessionValue) 
	    {
		LinSessionValue lsv = (LinSessionValue) sf;
		Channel channel = lsv.getLin() ;
		samLC(ch,channel,frame,ep,p_cont);
		p_cont.code = rhs;
		p_cont.frame = frame;
		p_cont.epnm = ep;		
	    } else {

	    IndexedSessionRef sref = (IndexedSessionRef)sf;
	    int doffset = sref.getOffset();
	    SessionRecord srec = sref.getSessionRec();

	    ASTNode cont = srec.getCont();
	    boolean pol = srec.getPol();
	    Env<SessionField> frm  = srec.getFrame();
	    Env<EnvEntry> epn  = srec.getFrameP();
	    boolean pold = srec.getPolDual();


	    if (rec) { // REC

		if (!pol) throw new SAMError("unfold neg "+ch);
			   
	    
		srec.writeSlot(new SessionFieldUnfold(),doffset);
		sref.incOffset();
		
		if (CLLSj.trace) {
		    System.out.println("unfold-op  "+ ch +" "+srec+" @ "+doffset);
		}
				
		srec.setPolDual(tyrhs.isPos(ep));  
		srec.setPol(false); 

		srec.setCont(rhs);
		srec.setcch(ch);
		srec.setFrame(frame);
		srec.setFrameP(ep);

		p_cont.code = cont;
		p_cont.frame = frm;
		p_cont.epnm = epn;
		return;
		
	    }
	
	    else { // COREC
						    
		if (pol)  {
		
		    if (pol) throw new SAMError("co-unfold pos");
		
		    if (CLLSj.trace) {
			System.out.println("co-unfold-op S [-] "+ch);
		    }
				
		    srec.setPolDual(srec.getPol());  
		    srec.setPol(pold); 

		    srec.setCont(this);
		    srec.setcch(ch);
		    srec.setFrame(frame);
		    srec.setFrameP(ep);
		    p_cont.code = cont;
		    p_cont.frame = frm;
		    p_cont.epnm = epn;
		    return;
	   

		} else  {
		
		    if (CLLSj.trace) {
			System.out.println("co-unfold-op "+ ch+" "+srec+" @ "+doffset);
		    }

		    //System.out.println("SessionValue = "+srec.readSlot(doffset).show());;
		
		    SessionFieldUnfold arg = (SessionFieldUnfold)srec.readSlot(doffset);

		    srec.writeSlot(null,doffset);  // reset linear value!
		    sref.incOffset();

		    // reset session

		    IndexedSessionRef srefd = (IndexedSessionRef)srec.getFrame().find(srec.getcch());
		    // System.out.println("e="+sref.getOffset()+" d="+srefd.getOffset());
		    sref.resetOffset();
		    srefd.resetOffset();

		    // switch to unroll buffer

		    if (tyrhs.isPos(ep)) { 
				
			srec.setPolDual(false);  
			srec.setPol(true);
		    
			p_cont.code = rhs;
			p_cont.frame = frame;
			p_cont.epnm = ep;
		    
			return;
		    } else {
		    
			srec.setPolDual(false /* pol */);  
			srec.setPol(true /* pold */);
		
			// srec.setPolDual(tyrhs.isPos(ep));

			// System.out.println("switchto pol = "+pold + " pold=" +pol+" prhs ="+ tyrhs.isPos(ep));		
			srec.setCont(rhs);
			srec.setcch(ch);
			srec.setFrame(frame);
			srec.setFrameP(ep);
			p_cont.code = cont;
			p_cont.frame = frm;
			p_cont.epnm = epn;
			return;
	    
		    } 

		} 
	    } 

	} 

	    
    }

    public void show()  {
	System.out.println(this+" "+ch+" "+anc);
	rhs.show();
    }

}
