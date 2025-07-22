import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTWhy extends ASTNode {
    String ch;
    ASTNode rhs;
    ASTType __type;

    public ASTWhy(String _ch, ASTNode _rhs) {
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

    public  void ASTInsertUse(String _ch,  ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	anc.ASTInsertUse(_ch,t,this,disCont);
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	ASTNode pushCall = new ASTCall(ch, cho, t,here);
	pushCall.eg = eg;
	here.setanc(pushCall);
	pushCall.setanc(this);
	rhs = pushCall;
    }

    public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception
    {
	anc.ASTInsertWhyNot(ch, _t, this);
    }
    
    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ,boolean exp) throws Exception
    {
	rhs = rhs.ASTweakeningOnLeaf(_ch,typ,exp);
	return this;
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	rhs = newCont;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    	this.eg = eg;

	this.inferUses(ch,ed,ep);

	//System.out.println("TC WHY contType "+ch);

	ASTType ty = ed.find(ch);
	ty = ty.unfoldType(ep);
        ty = ASTType.unfoldRec(ty);
	if (ty instanceof ASTWhyT) {
	    ASTWhyT tyr = (ASTWhyT)ty;
	    ed.upd(ch,null);
	    __type = tyr.getin().unfoldType(ep);
	    //		eg=eg.assoc(ch, type);
	    eg=eg.assoc(ch, __type);
	    rhs.typecheck(ed,eg,ep);
	} else if (ty instanceof ASTCoBasicType) {
	    ASTCoBasicType tyr = (ASTCoBasicType)ty;
	    ed.upd(ch,null);
	    eg=eg.assoc(ch, tyr.lift());
	    rhs.typecheck(ed,eg,ep);
	}
	else throw new TypeError("Line " + lineno + " :" +"?: "+ch+" is neither of ? or basic co-type.");
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
	ASTWhy p = new ASTWhy(ch,rhs.subst(e));
	p.rhs.setanc(p);
	return p;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
	if(y == ch)
	    ch = x;

	rhs.subs(x,y);
    }

    public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger) throws Exception{
	Channel channel = (Channel) ed.find(ch);
	Server server = (Server) channel.receive();

	// System.out.println("RUN-WHY "+ch);

	logger.info("Server activated on session " + channel.getId());
	rhs.runproc(ep, ed, eg.assoc(ch,server), logger);
    }

    public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception
    {

	IndexedSessionRef sref = (IndexedSessionRef)frame.find(ch);
	int doffset = sref.getOffset();
	SessionRecord srec = sref.getSessionRec();

	boolean pol = srec.getPol();
	if(pol) {
	    // polarity +
	    System.out.println("SAM-WHY "+ch+"@"+doffset+ " + WAIT");
	    System.exit(0);	     
	} else
	    {
		// polarity -
		SessionField sf = srec.readSlot(doffset);
		sref.incOffset();
		if (sf == null) throw new SAMError("SAM-WHY-read-FAILURE");
		srec.writeSlot(null,doffset);  // reset linear value
		frame.upd(ch, sf);
		rhs.sam(frame,ep);
	    }     	
    }

    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	SessionField sf0 = frame.find(ch);

	if (sf0 instanceof LinSessionValue) {
	    LinSessionValue lsv = (LinSessionValue) sf0;
	    Channel channel = lsv.getLin() ;
	    SessionClosure clos = (SessionClosure) channel.receive();
	    frame = frame.assoc(ch, clos);
	    p_cont.code = rhs;
	    p_cont.frame = frame;
	    p_cont.epnm = ep;
		
	} else {
	    IndexedSessionRef sref = (IndexedSessionRef)sf0;
	    int doffset = sref.getOffset();
	    SessionRecord srec = sref.getSessionRec();
	    boolean pol = srec.getPol();
	    if(!pol) {
		    if (CLLSj.trace) {
				System.out.println("why?-op "+ch+" "+srec+" @ "+doffset);
			}
			
				SessionField sf = srec.readSlot(doffset);
				if (sf == null) throw new SAMError("SAM-WHY-read-FAILURE");
				srec.writeSlot(null,doffset); 
				sref.incOffset();
				frame = frame.assoc(ch, sf);
				p_cont.code = rhs;
				p_cont.frame = frame;
				p_cont.epnm = ep;
				SessionRecord.freeSessionRecord(srec);   	
	    } else
		{
			throw new SAMError("SAM-WHY-UNEXPECTED-POLARITY");
		}     	
	}
    }

    
}
