import java.util.*;
import java.util.function.*;

public abstract class ASTType {

    // int offset;

    public abstract boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception ; 

    public  abstract ASTType dual(Env<EnvEntry> e) throws Exception;

    public abstract ASTType unfoldType(Env<EnvEntry> e) throws Exception;

    public abstract void kindcheck(Env<EnvEntry> e) throws Exception;

    public abstract String toStr(Env<EnvEntry> e) throws Exception;

    public abstract ASTType subst(Env<ASTType> e); 

    
    public  boolean equals(Object t0) {
	boolean rb = false;
	if (!(t0 instanceof ASTType)) return false;
	ASTType t = (ASTType)t0;
	try {
	    rb = this.equalst(t,null,false,null);
	} catch (Exception _e) {
	    System.out.println("ABORT EQUALS T");
	    _e.printStackTrace(System.out);
	    System.exit(0);
	}
	return rb;
	
    }

    static private int n = 0;
    static String gensym() {
	String s = "$"+(n++);
	return s;
    }

    static synchronized void reinit() {
	n = 0;
    }
    
    static ASTType unfoldRec(ASTType t) {
	if (t instanceof ASTRecT) {
	    ASTRecT t0 = (ASTRecT)t;
	    //System.out.println("auto-UNFOLD rec!"+t0.getin());
	    return unfoldRec(t0.getin());
	} else
	    if (t instanceof ASTCoRecT) {
		ASTCoRecT t0 = (ASTCoRecT)t;
		//System.out.println("auto-UNFOLD corec!"+t0.getin());
		return unfoldRec(t0.getin());
	    } else return t;
    }

    boolean sameTopType (ASTType t)  {
	return (this instanceof ASTRecT &&  t instanceof ASTRecT) ||
	    (this instanceof ASTCoRecT &&  t instanceof ASTCoRecT) ||
	    (this instanceof ASTCoAffineT &&  t instanceof ASTCoAffineT);
    }
    
    static ASTType unfoldRecInfer(ASTType t, ASTNode here, String ch, Env<EnvEntry> ep)
	throws Exception {
	ASTNode ancn = here.getanc();
	//System.out.println("RecInfer "+t+" # "+here);
	boolean done = (here instanceof ASTUse) || (here instanceof ASTDiscard);
	if(t instanceof ASTCoAffineT  && !done)
	    {
		ASTCoAffineT co0 = (ASTCoAffineT)t;
		ASTType tyco = co0.getin().unfoldType(ep);

		if(CLLSj.trace_level==5)
		    System.out.println("ASTCoAffine unfold infer "+here);

		ASTNode parent = here.getanc();
		Function<ASTNode,ASTNode> f = (ASTNode x) -> { ASTUse n = new
							       ASTUse(ch,x);
							       n.setrhs(tyco);
							       return n;};
		parent.ASTInsertPipe(f, here);
		return unfoldRecInfer(tyco,here,ch,ep);
	    } else {
	    done = (here instanceof ASTUnfold);
	    if (t instanceof ASTRecT &&  !done) // on c
		{
		    if(CLLSj.trace_level==5)
			System.out.println("ASTRec unfold infer "+here);
		    ASTRecT t0 = (ASTRecT)t;
		    ASTType tyco = t0.getin().unfoldType(ep);
		
	    
		    Function<ASTNode,ASTNode> f = (ASTNode x) -> {
			ASTUnfold n = new ASTUnfold(ch,x);
			n.rec = true;
			n.setrhs(tyco);
			return n;
		    };
		    ASTNode parent = here.getanc();
		    parent.ASTInsertPipe(f, here);

		    ASTUnfold unf = (ASTUnfold)here.getanc();
		    unf.rec = true;
	    
		    return unfoldRecInfer(tyco,here,ch,ep);
		} else {
		done = (here instanceof ASTUnfold);
		if (t instanceof ASTCoRecT  &&  !done)
		    {
			if(CLLSj.trace_level==5)
			    System.out.println("ASTCoRec unfold infer "+here+" "+CLLSj.trace_level);
			
			ASTCoRecT t0 = (ASTCoRecT)t;
			ASTType tyco = t0.getin().unfoldType(ep);

	    
			Function<ASTNode,ASTNode> f = (ASTNode x) -> { ASTUnfold n = new
								       ASTUnfold(ch,x);
								       n.rec = false;
								       n.setrhs(tyco);
								       return n;};
			ASTNode parent = here.getanc();
			parent.ASTInsertPipe(f, here);
			ASTUnfold unf = (ASTUnfold)here.getanc();
			unf.rec = false;
			return unfoldRecInfer(tyco,here,ch,ep);
		    } else {
		    return t;
		}
	    }
	}		    
    }
    
    static ASTType unfoldRecInferParameter(ASTType t, ASTType formal, ASTNode here, String ch, Env<EnvEntry> ep)
	throws Exception {
	ASTNode ancn = here.getanc();
	
	if (t.sameTopType(formal)) return t;
	
	if (t instanceof ASTRecT) // on c
	    {
		if(CLLSj.trace_level==5)
		    System.out.println("ASTRec unfold infer "+here);
		ASTRecT t0 = (ASTRecT)t;
		ASTType tyco = t0.getin().unfoldType(ep);
			    
		Function<ASTNode,ASTNode> f = (ASTNode x) -> {
		    ASTUnfold n = new ASTUnfold(ch,x);
		    n.rec = true;
		    n.setrhs(tyco);
		    return n;
		};
		ASTNode parent = here.getanc();
		parent.ASTInsertPipe(f, here);

		ASTUnfold unf = (ASTUnfold)here.getanc();
		unf.rec = true;
	    
		return unfoldRecInferParameter(tyco,formal,here,ch,ep);
	    } else {
	    if (t instanceof ASTCoRecT)
		{
			if(CLLSj.trace_level==5)
			    System.out.println("ASTCoRec unfold infer "+here+" "+CLLSj.trace_level);
			
			ASTCoRecT t0 = (ASTCoRecT)t;
			ASTType tyco = t0.getin().unfoldType(ep);

	    
			Function<ASTNode,ASTNode> f = (ASTNode x) -> { ASTUnfold n = new
								       ASTUnfold(ch,x);
								       n.rec = false;
								       n.setrhs(tyco);
								       return n;};
			ASTNode parent = here.getanc();
			parent.ASTInsertPipe(f, here);
			ASTUnfold unf = (ASTUnfold)here.getanc();
			unf.rec = false;
			return unfoldRecInferParameter(tyco,formal,here,ch,ep);
		} else {
		return t;
	    }
	}
    }
    
    static boolean affine(ASTType t, Env<EnvEntry> ep) throws Exception {
	// USED IN EXPERIMENTAL labeled tensors
	//System.out.println("affine ? "+t);
	if(t instanceof ASTAffineT) return true;
	else
	    if(t instanceof ASTCellT) return true;
	    else
		if (t instanceof ASTStructT) {
		    ASTStructT ty = (ASTStructT)t;
		    HashMap<String,ASTType> tmap = ty.getcases();
		    for ( Iterator<String>
			      is = tmap.keySet().iterator();is.hasNext();) {
			String lab = is.next();
			ASTType telem = tmap.get(lab);
			telem = telem.unfoldType(ep);
			telem = ASTType.unfoldRec(telem);
			if (!ASTType.affine(telem,ep)) return false;
		    }
		    return true;  	
		} else if (t instanceof ASTBasicType)
		    { return true; }
		else return false;		
    }

    
    // for SAM

    // public  int GetOffset() {
    //	return offset;
    // }
	
    public  boolean isPos(Env<EnvEntry> ep) throws Exception {
	ASTType it = this;
	it = it.unfoldType(ep);
	if (it instanceof ASTOneT ||
	    it instanceof ASTSendT ||
	    it instanceof ASTBangT ||
	    it instanceof ASTCaseT ||
	    it instanceof ASTSendTT ||
	    it instanceof ASTBasicType ||
	    it instanceof ASTRecT ||
	    it instanceof ASTAffineT ||
	    it instanceof ASTCellT
	    
	    ) {
	    return true;
	}
	else {
	    return false;
	}
    }

    public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
	System.out.println(this+"@SetOffsets UNDEFINED");
	System.exit(0);
	return -10; // unreachable
    }

}
