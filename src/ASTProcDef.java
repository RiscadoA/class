import java.util.*;
import java.util.function.*;

public class ASTProcDef extends ASTNode {
    String id;
    Env<EnvEntry> ep;
    ASTNode rhs;
    List<String> targs;
    List<String> targsGen;
    List<String> args;
    List<ASTType> argtypes;
    List<String> gargs;
    List<ASTType> gargtypes;
    boolean rec;
    boolean unsafe_rec;
    boolean rCall = false;

    public ASTProcDef(){
	targs = new ArrayList<String>();

	args = new ArrayList<String>();
	argtypes = new ArrayList<ASTType>();

	gargs = new ArrayList<String>();
	gargtypes = new ArrayList<ASTType>();
    }

    public ASTProcDef (String _id) {
	id = _id;

	targs = new ArrayList<String>();

	args = new ArrayList<String>();
	argtypes = new ArrayList<ASTType>();

	gargs = new ArrayList<String>();
	gargtypes = new ArrayList<ASTType>();

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

    public void show() {
	rhs.show();
    }

    public  void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception
    {
	ASTUse pushUse = new ASTUse(ch, here);
	pushUse.setrhs(t);
	pushUse.eg = eg;
  	here.setanc(pushUse);
	pushUse.setanc(this);
        rhs = pushUse;
    }

    public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception{
	ASTNode pushCall = new ASTCall(ch, cho, t,here);
	pushCall.eg = eg;
	here.setanc(pushCall);
	pushCall.setanc(this);
	rhs = pushCall;
    }

    public void XASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	throw new Exception ("Unexpected call: ASTInsertWhyNot"); // never called
    }

    public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception
    {
	ASTNode pushWhy = new ASTWhy(_ch, here);
	pushWhy.eg = eg;
	here.setanc(pushWhy);
	pushWhy.setanc(this);
	rhs = pushWhy;
	eg.insert(_ch,_t);
    }

    public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception{
	rhs = newCont;
    }
    
    public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception
    {
	throw new Exception ("Unexpected call: ASTweakeningOnLeaf"); // never called
    }

    public void setId(String _id){
    	id = _id;
    }

    public void setRec(boolean _rec){
	rec = _rec;
    }

    public void setUnsafe_Rec(boolean _unsafe_rec){
	unsafe_rec = _unsafe_rec;
    }

    public void setEnv(Env<EnvEntry> _ep){
	ep = _ep;
    }

    public Env<EnvEntry> getEnv(){
	return ep;
    }

    public void addDPar(String pid, ASTType t) throws Exception {
	if(targs.contains (pid) || args.contains(pid)) 
	    throw new SyntaxError("Parameter "+pid+" declared twice in "+id);
	args.add(pid);
	argtypes.add(t);

    }

    public void addGPar(String pid, ASTType t) throws Exception {
	if(targs.contains (pid) || args.contains(pid) || gargs.contains(pid)) 
	    throw new SyntaxError("Parameter "+pid+" declared twice in "+id);
	gargs.add(pid);
	gargtypes.add(t);

    }
    
    public void addTPar(String tid) throws Exception {
	if(targs.contains(tid)) 
	    throw new SyntaxError("Type parameter "+tid+" declared twice in "+id);
	targs.add(tid);
    }    

    public void setProc (ASTNode _rhs) {
	rhs = _rhs;
    }

    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

	// System.out.println("\nProc TC "+ id);
	// ep.crawl();
	
	ArrayList<String> newargs = new ArrayList<String>();
	ArrayList<ASTType> newargtypes = new ArrayList<ASTType>();
	ArrayList<ASTType> newgargtypes = new ArrayList<ASTType>();

	this.eg = eg;
	
	Env<ASTType> rho = new Env<ASTType>();

	targsGen = new ArrayList<String>(); //PR
	
	for (String param : targs) {

	    //PR: why not simply this line below?
	    //ep = ep.assoc(param, new TypeEntry(new ASTIdT(param)));

	    String fresh = ASTType.gensym();
	    
	    ASTType gs = new ASTIdT(fresh);

	    // System.out.println("GENSYM ProcDef = "+ ((ASTIdT)gs).getid());

	    EnvEntry gse = new TypeEntry(gs);
		
	    ep=ep.assoc(fresh,gse);

	    newargs.add(fresh);
	    rho = rho.assoc(param, new ASTIdT(fresh));

	    targsGen.add(fresh);

	    targs = newargs;
	}

	
	Iterator<ASTType> it = argtypes.iterator();
	for (String param : args) {
	    // System.out.println("ARGS");
	    ASTType argtype = it.next().subst(rho);
	    // System.out.println("argtype "+argtype.toStr(ep));
	    newargtypes.add(argtype);
	    // SAM
	    //argtype = argtype.unfoldType(ep);
	    //argtype.SetOffsets(0,ep);
	    //ed = ed.assoc(param, argtype);
	    ed = ed.assoc(param, argtype.unfoldType(ep));
	}

	argtypes = newargtypes;


       	Iterator<ASTType> itg = gargtypes.iterator();
	for (String param : gargs) {
	    ASTType argtype = itg.next().subst(rho);
	    newgargtypes.add(argtype);
	    eg = eg.assoc(param, argtype.unfoldType(ep));
	}
	
	gargtypes = newgargtypes;

	rhs = rhs.subst(rho);
	rhs.setanc(this);

	if (rec){
	    ASTType tfirst;
	    if(argtypes.isEmpty())
		throw new TypeError("Line " + lineno + " :"
				    +"Recursive process definition (" + id +
				    ") must have a first linear parameter.");
	    tfirst = argtypes.get(0).unfoldType(ep);
	    if(tfirst instanceof ASTCoRecT){
		//tfirst = ASTType.unfoldRec(tfirst);
		// tfirst = ASTType.unfoldRecInfer(tfirst, rhs, args.get(0)); // rhs was this
		this.rCall = true;
		ep = ep.assoc(args.get(0), new RVarEntry());
		ep = ep.assoc(id, new ProcEntry(this));
	    } else throw new TypeError("Line " + lineno + " :" +"Recursive process definition (" + id
				       + ") requires a first linear parameter of corecursive type, found "+tfirst.toStr(ep));
	}

	if(unsafe_rec)
	    ep=ep.assoc(id,new ProcEntry(this));

	// System.out.println("GO TC");

	rhs.typecheck(ed,eg,ep);
	

	/*	if(rec)
		argtypes.set(0, tfirst);*/

	for (String param : args) {
	    rhs = ASTInferLinClose(rhs,param,ed,ep);
	}
	
	if(rec)	this.rCall = false;

	//	rhs.show();
	
    }

    public Set<String> fn(Set<String> s) {
	return s;
    }

    public Set<String> fnLinear(Set<String> s) {
	return s;
    }

    public ASTNode subst(Env<ASTType> e) {
	return this;
    }

    public void subs(String x, String y){// implements x/y (substitutes y by x)
    }


    
}
