import java.util.*;
import java.util.logging.*;
import java.util.function.*;
   
public class ASTClose extends ASTNode {

    String ch;

    public ASTClose(String _ch) {
	ch = _ch;
    }

    public void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {	throw new Exception("ASTInsertPipe: call not expected");
    }

    public  void ASTInsertUse(String ch, ASTType _t, ASTNode here, Boolean disCont) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertUse"); // never called
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	throw new Exception ("Unexpected call: ASTInsertCall"); // never called
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
	throw new Exception ("Unexpected call.");
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception
    {
	return this.ASTweakeningTerm(_ch,exp);		
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	this.eg = eg;
	this.inferUses(ch,ed,ep);
	ASTType ty;
	ty = ed.find(ch);
	ty = ty.unfoldType(ep);
	if (ty instanceof ASTOneT) {
	    ed.upd(ch,null); 
	} else throw new TypeError("Line " + lineno + " :" +"CLOSE: "+ch+" is not of CLOSE type: found "+ty.toStr(ep));
    }

    public Set<String> fn(Set<String> s) {
	s.add(ch);
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	s.add(ch);
	return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	return this;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
	if(y == ch)
	    ch = x;
    }

    public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger) throws Exception{
	Channel channel = (Channel) ed.find(ch);
	channel.send("END");

    }

    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception			   
    {
	SessionField srefi = frame.find(ch); // concurrent channel
	if (srefi instanceof LinSessionValue)  {
		if (CLLSj.trace) {
			System.out.println("cclose-op pre "+ch);
			}
    ((Channel)(((LinSessionValue)srefi).getLin())).send("END");
	    if (CLLSj.trace) {
			System.out.println("cclose-op post "+ch);
			}
		p_cont.code = null;
	} else 
	    { 
		IndexedSessionRef sref = (IndexedSessionRef)frame.find(ch);
		int doffset = sref.getOffset();
		SessionRecord srec = sref.getSessionRec();

	  
		if (srec.getPol()) 
		    {   

			if (CLLSj.trace) {
			    System.out.println("clos-op "+ch+" "+srec+" @ "+doffset);
			}

			srec.writeSlot(CloseTok,doffset);
			sref.incOffset();

			ASTNode cont = srec.getCont();
			Env<SessionField> frm = srec.getFrame();
			Env<EnvEntry> epn  = srec.getFrameP();
			boolean pold = srec.getPolDual();
		
			srec.setPolDual(srec.getPol()); 
			srec.setPol(false); // polarity for other endpoint
		
			srec.setcch(ch);
			srec.setCont(null);
		
			srec.setFrame(frame);
			srec.setFrameP(ep);
		
			p_cont.code = cont;
			p_cont.frame = frm;
			p_cont.epnm = epn;

		
		    } else
		    {
			throw new SAMError("clos-op - "+ch);
		    }
	    }

    }

    
}
