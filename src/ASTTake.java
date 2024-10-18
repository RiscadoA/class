import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTTake extends ASTNode {
    String chr;
    String chi;
    ASTType type;
    ASTType ty_lhs; 
    ASTNode rhs;

    public ASTTake (String _chr,String _chi,ASTType _type,ASTNode _rhs) {
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

    public  void ASTInsertUse(String _ch,ASTType t,  ASTNode here, Boolean disCont) throws Exception
    {
	if(_ch.equals(chi)) {
	    ASTUse pushUse = new ASTUse(_ch, here);
	    pushUse.setrhs(t);
	    pushUse.eg = eg;
	    here.setanc(pushUse);
	    pushUse.setanc(this);
	    rhs = pushUse;
	}else
	    anc.ASTInsertUse(_ch, t, this, disCont);
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
	if(_ch.equals(chi)) {
	    if (!_ch.equals(chr)) {
		if(exp) {
		    return this.ASTInsertWhy(_ch);
		} else {
		    return this.ASTInsertMixDiscard(_ch);
		}		
	    }
	    throw new TypeError("Line " + lineno + " :" +"for " + _ch + " type pending after TAKE on "+chr);
	}
	rhs = rhs.ASTweakeningOnLeaf(_ch,typ, exp);
	return this;
    }
   
    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	anc.ASTInsertWhyNot(_ch, _t, this);
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	rhs = newCont;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	this.eg = eg;

	// this.inferUses(chr,ed,ep);

	ASTType typee = null;
	ASTType ty = ed.find(chr);
	if (type != null) {
	    typee = type.unfoldType(ep);
	    //typee = ASTType.unfoldRec(typee);
	};
	ty = ty.unfoldType(ep);
	ty = ASTType.unfoldRecInfer(ty, this, chr,ep);

	if (ty instanceof ASTUsageT) {
	    ASTUsageT tyr = (ASTUsageT)ty;
	    ty_lhs =  new ASTCoAffineT(tyr.getin());
	    if(typee != null && !typee.equalst(ty_lhs,ep,true, new Trail()))
		throw new TypeError("Line " + lineno + " :" +"TAKE "+chi
				    +" type mismatch: found="+ty_lhs.toStr(ep)+" declared="+typee.toStr(ep));
	    Env<ASTType> ext = ed.assoc(chi, ty_lhs);
	    ext.upd(chr, new ASTUsageLT(tyr.getin().unfoldType(ep)));
	    ep = ASTNode.propagateRVar(ep, chr, chi);
	    //  System.out.println("HERE -TAKE");
	    rhs.typecheck(ext,eg,ep);
		 
	    rhs.linclose(ed,ep);
	       	       
	    rhs = ASTInferLinClose(rhs,chi,ext,ep);
	       
	}
	else throw new TypeError("Line " + lineno + " :" +"TAKE: "+chr+" is not of USAGE type.");
    }

    public Set<String> fn(Set<String> s) {
	s.add(chr);
	s = rhs.fn(s);
	s.remove(chi);
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	s.add(chr);
	s = rhs.fnLinear(s);
	s.remove(chi);
	return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	ASTType ts = (type==null)?type:type.subst(e);
	ASTTake p =  new ASTTake(chr,chi,ts,rhs.subst(e));
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

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed,
			Env<Server> eg, Logger logger) throws Exception {
	Cell cell = (Cell) ed.find(chr);
	LinSession session = cell.take(chi);
	logger.info("TAKE cell " + cell.getId() + " on session " + chi);
	rhs.runproc(ep, ed.assoc(chi, session), eg, logger);

    }
    
    public void samLTake(MVar var, SessionClosure clos, Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont)
	throws Exception {
	if (CLLSj.trace) {
	    System.out.println("take-op "+chr+" "+clos);
	} 
	String id = clos.getId();
	Env<SessionField> frameloc = clos.getEnv();
	Env<EnvEntry> framep = clos.getEnvP();
	SessionRecord sreco = SessionRecord.newSessionRecord(clos.getSize());
	IndexedSessionRef srecfw = new IndexedSessionRef(0,sreco);
	IndexedSessionRef srecfr = new IndexedSessionRef(0,sreco);
	if (!ty_lhs.isPos(ep)) { // recv arg is writer U;T U negative
	    // System.out.println(" recv arg is writer U;T U negative");
	    Env<SessionField> fwrite = frameloc.assoc(id,srecfw);
	    Env<SessionField> fread = frame.assoc(chi,srecfr);
	    fread = fread.assoc(chr,var);
	    sreco.setPol(true); 
	    sreco.setPolDual(false); 
	    sreco.setcch(chi);
	    sreco.setCont(rhs);
	    // System.out.println("CONT TAKE arg writes "+rhs);
	    sreco.setFrame(fread);
	    sreco.setFrameP(ep);
	    p_cont.code = clos.getBody();
	    p_cont.frame = fwrite;
	    p_cont.epnm = framep;
	    return;
	} else { // recv arg is reader, U;T U positive
	    Env<SessionField> fwrite = frame.assoc(chi,srecfw);
	    Env<SessionField> fread = frameloc.assoc(id,srecfr);
	    fwrite = fwrite.assoc(chr,var);
	    sreco.setPol(true); 
	    sreco.setPolDual(false); 
	    sreco.setcch(id);
	    sreco.setCont(clos.getBody());
	    System.out.println("CONT TAKE arg reads"+clos.getBody());
	    sreco.setFrame(fread);
	    sreco.setFrameP(framep);
	    p_cont.code = rhs;
	    p_cont.frame = fwrite;
	    p_cont.epnm = ep;
	    return;
	}
    }

											   
    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	SessionField sf = (SessionField)frame.find(chr);

	if (sf instanceof MVar) 
	    {
		MVar cell = (MVar)sf;
		SessionClosure clos = cell.take();
		if (clos == null) throw new SAMError("SAM-TAKE-read-FAILURE");
		samLTake(cell,clos,frame,ep,p_cont);
		return;
	    }	else
	    {
		IndexedSessionRef sref = (IndexedSessionRef)sf;
		int doffset = sref.getOffset();
		SessionRecord srec = sref.getSessionRec();	    	
		if (!srec.getPol())
		{
		    MVar cell = (MVar)srec.readSlot(doffset);
		    if (cell == null)
			throw new SAMError("SAM-TAKE-read-FAILURE "+chr+ " "+srec);

		    srec.writeSlot(null,doffset);
		    sref.incOffset();
		    SessionClosure clos = cell.take();
		    samLTake(cell, clos,frame,ep,p_cont);
		    return;
		}  else throw new SAMError("take-op - "+chr+" "+srec);
	    }
    }    
}
