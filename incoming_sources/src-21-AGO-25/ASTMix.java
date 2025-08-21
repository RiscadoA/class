import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTMix extends ASTNode {

    ASTNode lhs;
    ASTNode rhs;
    boolean con;
    public ASTMix(boolean _con, ASTNode _lhs,ASTNode _rhs) {
	lhs = _lhs;
	rhs = _rhs;
	con = _con;
    }

    public  void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	anc.ASTInsertUse(ch,t, this,disCont);
    }

    public  void ASTInsertPipe(Function<ASTNode,ASTNode> f, ASTNode from) throws Exception
    {
	if (from==lhs) { ASTNode nnode = f.apply(from);
	    lhs.setanc(nnode);
	    lhs = nnode;
	    nnode.setanc(this);
	} else if (from==rhs) { ASTNode nnode = f.apply(from);
	    rhs.setanc(nnode);
	    rhs = nnode;
	    nnode.setanc(this);
	} else
	    {
		throw new Exception("ASTInsertPipe: call not expected");   
	    }
    }
    public  void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception
    {
	ASTNode pushCall = new ASTCall(ch, cho, t, here);
	pushCall.eg = eg;

	here.setanc(pushCall);
	pushCall.setanc(this);
	if (lhs==here) {
	    lhs = pushCall;
	}
	else {	rhs = pushCall;
	}
    }

    public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception
    {
	anc.ASTInsertWhyNot(ch, _t, this);
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	if(caller == lhs)
	    lhs = newCont;
	else
	    rhs = newCont;
    }

    public ASTNode ASTweakeningOnLeaf(String ch, ASTType t, boolean exp) throws Exception
    {
	Set<String> s = lhs.fn(new HashSet<String>());
	if ( s.contains(ch)  ) {
	    lhs = lhs.ASTweakeningOnLeaf(ch,t, exp);
	    return this;
	};
	rhs = rhs.ASTweakeningOnLeaf(ch,t, exp);
	return this;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
	this.eg = eg;
	Env<ASTType> eglhs = eg.assoc("$DUMMY",new ASTBotT());
	lhs.typecheck(ed,eglhs,ep);
	lhs.linclose(ed,ep);
	Env<ASTType> egrhs = eg.assoc("$DUMMY",new ASTBotT());
	rhs.typecheck(ed,egrhs,ep);
	rhs.linclose(ed,ep);
    }

    public Set<String> fn(Set<String> s) {
	s = lhs.fn(s);
	s = rhs.fn(s);
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	s = lhs.fnLinear(s);
	s = rhs.fnLinear(s);
	return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	ASTMix p = new ASTMix(con,lhs.subst(e), rhs.subst(e));
	p.lhs.setanc(p);
	p.rhs.setanc(p);
	return p;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
	lhs.subs(x,y);
	rhs.subs(x,y);
    }

    /*
      Calls runproc on lhs and rhs in parallel
    */
    public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger) throws Exception{
	CLLSj.threadPool.submit(
				new Runnable(){
				    public void run(){ try {
					    lhs.runproc(ep, ed, eg, logger);
					} catch (Exception e) {e.printStackTrace(System.out);} }
				});
	rhs.runproc(ep, ed, eg, logger);
    }

    public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception
    {
	lhs.sam(frame,ep);
	rhs.sam(frame,ep);
    }

    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	if (CLLSj.trace) {
	    System.out.println("cmix-op ");
	}
	
	if (con)  {
	    Env<SessionField> flhs = frame.assoc("$DUMMY",null);
	    Env<SessionField> frhs = frame.assoc("$DUMMY",null);
	    CLLSj.threadPool.submit(
				    new Runnable(){
					public void run(){ try {
						SAM.SAMloop(lhs,flhs, ep);
					    } catch (Exception e) {e.printStackTrace(System.out);} }
				    });
	    p_cont.code = rhs;
	    p_cont.frame = frhs;
	    p_cont.epnm = ep;
	    
	} else  {	    
	    /* sequentialize */
	    SAM.SAMloop(lhs,frame,ep);
	    p_cont.code = rhs;
	    p_cont.frame = frame;
	    p_cont.epnm = ep;
	}
    }

    
}
