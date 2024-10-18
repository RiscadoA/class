import java.util.*;
import java.util.logging.*;
import java.util.function.*;

public class ASTAffine extends ASTNode {
    String ch;
    ASTNode rhs;

    HashMap<String,ASTType> usageSet;
    HashMap<String,ASTType> coaffineSet;
    ASTType contType;

    public ASTAffine (String _ch,ASTNode _rhs) {
        ch = _ch;
        rhs = _rhs;
        usageSet = new HashMap<String,ASTType>();
        coaffineSet = new HashMap<String,ASTType>();
    }

    public boolean isCoaffine (ASTCoStructT ty) {
	return false;
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
    
    public  void ASTInsertUse(String _ch, ASTType _t, ASTNode here, Boolean disCont) throws Exception
    {
	if(disCont && !_ch.equals(ch))
	    anc.ASTInsertUse(_ch,_t, this,disCont);
	else {
	    ASTUse pushUse = new ASTUse(_ch, here);
	    pushUse.eg = eg;
	    pushUse.setrhs(_t);
	    here.setanc(pushUse);
	    pushUse.setanc(this);
	    rhs = pushUse;
	}
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
        if(_ch.equals(ch)) {
            ASTNode pushWhy = new ASTWhy(_ch, here);
            pushWhy.eg = eg; 
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

    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ,boolean exp) throws Exception
    {
	if (exp)
	    return this.ASTweakeningHere(_ch,  exp); // must insert here because affine blocks linear names ?x
	else {
	    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
	    // if(coaffineSet.contains(_ch)) System.out.println("in CoaffineSet");
	    coaffineSet.put(_ch,typ); 
	    return this;
	}
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
        this.eg = eg;
	//  this.inferUses(ch,ed,ep);
        ASTType ty = ed.find(ch);
        ty = ty.unfoldType(ep);
	ty = ASTType.unfoldRecInfer(ty, this, ch, ep);
        if (ty instanceof ASTAffineT) {
	   		
            Set<String> s = rhs.fn(new HashSet<String>());
            s.remove(ch);
            Iterator<String> it = s.iterator();
	    while(it.hasNext()){
                String id = it.next();
                ASTType tyId = null;
                try {
                    tyId = ed.find(id);
                } catch(Exception e) {}
                if (tyId != null) {
                    tyId = tyId.unfoldType(ep);
		    // tyId = ASTType.unfoldRec(tyId);
		    if (tyId instanceof ASTRecT || tyId instanceof ASTCoRecT)
			System.out.println(" REC AFFINE "+tyId.toStr(ep));

                    if (tyId instanceof ASTUsageT) {
			//System.out.println("\n\nusage-free "+ch+" ->"+id);
                        usageSet.put(id,tyId);
		    } 
                    else if (tyId instanceof ASTCoAffineT) {
			//System.out.println("\n\nco-affine-free "+ch+" ->"+id);
			coaffineSet.put(id,tyId);
		    } 
                    else if (tyId instanceof ASTWhyT) {
                        ASTWhyT t = (ASTWhyT) tyId;
                        tyId = t.getin();
                        this.getanc().ASTInsertWhyNot(id,tyId, this);
                        ed.updmove(id);
                    }
                    else
                        throw new TypeError("Line " + lineno + " :" +"AFFINE " + ch + ": " + id +
					    " is not USAGE or COAFFINE type: found "+ tyId.toStr(ep));
                } else {
		    tyId = eg.find(id); // just to make sure declared in Gamma
		};
            };
            ASTAffineT tyr = (ASTAffineT)ty;
	    contType = tyr.getin().unfoldType(ep);
	    ed.upd(ch, contType);
            rhs.typecheck(ed,eg,ep);
            rhs.linclose(ed,ep);
        }
        else throw new TypeError("Line " + lineno + " :" +"AFFINE: "+ch+" is not of AFFINE type: found "+ty.toStr(ep));
    }

    public Set<String> fn(Set<String> s) {
        s = rhs.fn(s);
        s.add(ch);
        return s;
    }

    public Set<String> fnLinear(Set<String> s) {
        s = rhs.fnLinear(s);
        s.add(ch);
        return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	ASTAffine p = new ASTAffine(ch,rhs.subst(e));
        p.lineno = this.lineno; 
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
	CLLSj.inc_affines(+1);
	//        System.out.println("+AFFINE:  "+ch);
        String selection = (String) channel.receive();
	//	CLLSj.inc_affines(-1);
        //       System.out.println("-AFFINE: " + selection +" "+ch);

        if(selection.equals("USE")){
            rhs.runproc(ep,ed,eg,logger);
        }
	else if (selection.equals("DISCARD")) {

	    //System.out.println("usage: ");
	    
            for (Map.Entry<String, ASTType> itU : usageSet.entrySet()) {
                String id = itU.getKey();	
                CLLSj.threadPool.submit( new Runnable(){
			public void run(){ try {
				// System.out.println("\n\nusageASTRelease: "+id+" "+channel);
				(new ASTRelease(id)).runproc(ep, ed, eg, logger);
			    } catch (Exception e) {e.printStackTrace(System.out);} }
		    });
            }


	    for (Map.Entry<String, ASTType> itU : coaffineSet.entrySet()) {
		String id = itU.getKey();	

                CLLSj.threadPool.submit( new Runnable(){
			public void run(){ try {
				// System.out.println("\n\nusageASTDiscard: "+id+" "+channel);
				(new ASTDiscard(id)).runproc(ep, ed, eg, logger);
			    } catch (Exception e) {e.printStackTrace(System.out);} }
		    });
            }
	    

        } else new RunError("panic: unexpected affine msg");

    }

    public String toStr(Env<EnvEntry> ep) throws Exception {
        return "affine " + ch + ";\n" + rhs.toStr(ep);
    }

    public void show()  {
	System.out.println(this+" "+ch+" "+anc);
	rhs.show();
    }

    
    public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception
    {
	SessionField sf = (SessionField)frame.find(ch);
	if (sf instanceof LinSessionValue) 
	    {
		LinSessionValue lsv = (LinSessionValue) sf;
		Channel channel = lsv.getLin() ;
		SessionFieldAffine affval = new SessionFieldAffine(coaffineSet);
		if (CLLSj.trace) {
		    System.out.println("affine-op-lc "+ch+" "+channel);
		}
		//System.out.println("+AFFINE:  "+ch);
		//CLLSj.inc_affines(+1);
		channel.send(affval);
		//CLLSj.inc_affines(-1);
		//System.out.println("-AFFINE:  "+ch);
		//System.out.println("++AFFINE ACK:  "+ch);
		SessionField ack = (SessionField)channel.receive();
		//System.out.println("--AFFINE ACK:  "+ch+" "+ack);
		if (ack instanceof SessionFieldUse) {
		    p_cont.code = rhs;
		    p_cont.frame = frame;
		    p_cont.epnm = ep;
		} else
		    if (ack instanceof SessionFieldDiscard) {
			// discard coaffine resources
			// System.out.println("affine-op-lc-discard "+ch+" "+channel);
			if (coaffineSet.size()>0) {
			    for (Map.Entry<String, ASTType> itU : coaffineSet.entrySet()) {
				String drop = itU.getKey();
				// System.out.println("affine-op-lc-discard-drop "+drop);
				ASTNode code = new ASTDiscard(drop);
				p_cont.code = null;
				p_cont.frame = frame;
				p_cont.epnm = ep;
				code.samL(frame,ep, p_cont) ;
			    } 
			} else
			p_cont.code = null;
		    }
		    else throw new SAMError("SAM-COAFFINE-read-FAILURE");
	    } else {
	    
	    IndexedSessionRef sref = (IndexedSessionRef)sf;
	    int doffset = sref.getOffset();
	    SessionRecord srec = sref.getSessionRec();
	
	    if (srec.getPol()) 
		{   

		    if (CLLSj.trace) {
			System.out.println("affine-op "+ch+" "+srec+" @ "+doffset);
		    }

		    SessionFieldAffine affval = new SessionFieldAffine(coaffineSet);

		    srec.writeSlot(affval,doffset);
		    sref.incOffset();

		    ASTNode cont = srec.getCont();
		    Env<SessionField> frm = srec.getFrame();
		    Env<EnvEntry> epn  = srec.getFrameP();
	    		
		    srec.setPolDual(true); // will be reset on use
		    srec.setPol(false); 

		    srec.setcch(ch);
		    srec.setCont(rhs);
		    srec.setFrame(frame);
		    srec.setFrameP(ep);

		    p_cont.code = cont;
		    p_cont.frame = frm;
		    p_cont.epnm = epn;

		} else
		{
		    throw new SAMError("affine-op - "+ch+" "+srec);
		}
	}


    }		    

    

}
