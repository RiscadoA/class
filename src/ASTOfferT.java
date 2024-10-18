import java.util.*;

public class ASTOfferT extends ASTType {
    HashMap<String,ASTType> cases;

    public ASTOfferT() {
	cases = new HashMap<String,ASTType> ();
    }

    public void addCase(String id, ASTType t) throws Exception{
	if(cases.putIfAbsent(id,t) != null)
	    throw new SyntaxError("Duplicate Label "+id+" in offer type");
    }

    HashMap<String,ASTType> getcases() {
	return cases;
    }
    
    public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
	t = t.unfoldType(e);
	//	t = ASTType.unfoldRec(t);
	//	System.out.println("OFFER equalst "+this + " "+t);
	if(t instanceof ASTOfferT) {
	    ASTOfferT w = (ASTOfferT)t;
	    if (cases.size() != w.getcases().size()) return false;
	    // System.out.println("OFFER # cases matched");
	    for ( Iterator<String>
		      is = cases.keySet().iterator();is.hasNext();) {
		String lab1 = is.next();
		ASTType t1 = w.getcases().get(lab1);
		if (t1==null) return false;
		if (!t1.equalst(cases.get(lab1),e,lit,trail)) return false;
		};
	    return true; 
	}
	return false;
    }

    public void kindcheck(Env<EnvEntry> e) throws Exception
    {
	    for ( Iterator<String>
		      is = cases.keySet().iterator();is.hasNext();) {
		String lab1 = is.next();
		ASTType t1 = cases.get(lab1);
		t1.kindcheck(e);
	    };
    }

    public ASTType dual(Env<EnvEntry> e) {
	ASTCaseT to = new ASTCaseT();
	for (Iterator<String> is = cases.keySet().iterator();is.hasNext();) {
	    String lab1 = is.next();
	    ASTType t1 = cases.get(lab1);
	    try {
		to.addCase(lab1,t1.dual(e));
	    } catch (Exception never) {}
	};
	return to;
    }

    public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
	/*	ASTOfferT to = new ASTOfferT();
	for (Iterator<String> is = cases.keySet().iterator();is.hasNext();) {
	    String lab = is.next();
	    ASTType t = cases.get(lab);
	    to.addCase(lab,t.unfoldType(e));
	};
	return to;
	*/
	return this;
    }

    public String toStr(Env<EnvEntry> e) throws Exception {
	String s = "OFFER OF {";
	for ( Iterator<String> is = cases.keySet().iterator(); is.hasNext();) {
		String lab1 = is.next();
		ASTType t1 = cases.get(lab1);
                s = s + "| "+lab1+" : "+t1.toStr(e);
	};
	return s+" }";
    }
    
    public  ASTType subst(Env<ASTType> e) {
	ASTOfferT ts = new ASTOfferT();
	for ( Iterator<String> is = cases.keySet().iterator(); is.hasNext();) {
		String lab1 = is.next();
		ASTType t1 = cases.get(lab1);
		try {
		    ts.addCase(lab1, t1.subst(e));
		} catch (Exception never) { System.out.println("SUBST OFFER EX"); System.exit(0);}
	};
	return ts;
    }
    
    public int SetOffsets(int base, Env<EnvEntry> ep)
    {
	int max = -1;
	// offset = base;
	for ( Iterator<String> is = cases.keySet().iterator();is.hasNext();) {
		String lab1 = is.next();
		ASTType t1 = cases.get(lab1);
		try {
		    int tag = t1.SetOffsets(base+1, ep);
		    if (tag > max) max = tag;
		} catch (Exception never) {}
	};
	return max;
    }
    
}
