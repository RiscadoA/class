
public class Env<X> {

    Env<X> anc;

    public int level = 0;

    String id = "$BOT$";

    X assoc;

    Env<X> assocs;

    boolean moved = false; // used for insertWhy

    public Env() {}
    
    public Env(String _id, X _assoc, Env<X> _anc) {
	id = _id;
	assoc = _assoc;
	anc = _anc;
	//	if (_anc!=null) level = _anc.level + 1;
    }

    public Env(String _id, X _assoc, Env<X> _assocs, Env<X> _anc, boolean _moved){
    	id =_id;
    	assoc = _assoc;
    	assocs = _assocs;
    	anc = _anc;
    	moved = _moved;
    }

    synchronized public Env<X> assoc(String id, X assoc) {
	return new Env<X>(id,assoc,this);
    }
    
    synchronized public void  insert(String _id, X _assoc) {
	if (assocs == null) assocs = new Env<X>();
	assocs = assocs.assoc(_id,_assoc);
    }

    synchronized public Env<X> shallowDup(){
	return new Env<X>(id, assoc, assocs, anc, moved);
    }

    synchronized public Env<X> dup() {
	Env<X>  de= new Env<X>(id,assoc,(anc==null)?null:anc.dup());
	return de;
    }

    synchronized public Env<X> dupe() {
	Env<X>  de= new Env<X>(id,assoc,(anc==null)?null:anc.dup());
	if (assocs != null) {
	    de.assocs = assocs.dup();
	}
	return de;
    }

    synchronized public boolean eq(Env<X> e0) {
	if (anc!=null && !anc.eq(e0.anc)) return false;
	return (assoc == e0.assoc);
    }

    synchronized public String findI(SessionRecord _it) throws Exception {
	if (assocs!=null)
	    try { // try overflow list (post-inference of ?x)
		String elt = assocs.findI(_it); return elt;
	    } catch (Exception e) {
	    };
	try {
	    if (((IndexedSessionRef)assoc).getSessionRec() == _it) {
		return id;
	    };
	} catch (Exception e) {}
	if (anc!=null) return anc.findI(_it);
	else throw new TypeError(_it+" not found (findI).");
    }		   

    
    synchronized public X find(String _id) throws Exception {
	if (assocs!=null)
	    try { 
		X elt = assocs.find(_id); return elt;
	    } catch (Exception e) {
	    }
	if (id.equals(_id)) {
	    if(assoc!=null) return assoc;
	    else throw new TypeError(_id+" -> null; not declared.");	
	};
	if (anc!=null) return anc.find(_id);
	else throw new TypeError(_id+" not declared.");
    }		   

    synchronized public boolean def(String _id) throws Exception {
	if (_id.equals(id)) { 
	    return (assoc!=null);
	}
	if (anc!=null)
	    return anc.def(_id);
	else throw new TypeError(_id+" not declared.");
    }

    synchronized public void upd(String _id, X _assoc) throws Exception {
	if (_id.equals(id)) assoc = _assoc;
	else {
	    if (anc!=null) anc.upd(_id, _assoc);
	    else throw new TypeError(_id+" not declared.");
	}
    }

    synchronized public void updmove(String _id) throws Exception {
	if (_id.equals(id)) {
	    moved = true;
	    assoc = null;
	}
	else {
	    if (anc!=null) anc.updmove(_id);
	    else throw new TypeError(_id+" not declared.");
	}
    }

    synchronized public void updmove(Env<X> env2) throws Exception {
	if (moved) {
	    env2.upd(id,null);
	    moved = false;
	}
	if (anc!=null) anc.updmove(env2);
    }
	
    synchronized public void crawl() {
	if (assocs!=null) {
	    System.out.print("\ncrawl::*");
	    assocs.crawl();
	    System.out.print("*::");
	}
	System.out.print(": "+id+" -> ");
	if (moved) System.out.print("M"); else System.out.print(" ");
	if (assoc == null) System.out.println("NULL");
	else
	    if (assoc!=null) {
		if (assoc instanceof TypeEntry ) {
		    try {
			System.out.println(((TypeEntry)assoc).getType().toStr((Env<EnvEntry>)this));
		    } catch (Exception _e) { System.out.println("X "); };
		} else if (assoc instanceof TypeDefEntry ) {
		    try {
			ASTTypeDef tn = ((TypeDefEntry)assoc).getTypeDef();
			Env<EnvEntry> ep = (Env<EnvEntry>)this;
			System.out.print("(");
			for (String param : tn.args) {
			    System.out.print(param+" ");
			    ep = ep.assoc(param, new TypeEntry (new ASTIdT(param)));
			}
			System.out.print(")"+tn.rhs+" ");
			System.out.println(tn.rhs.toStr(ep));
		    } catch (Exception _e) { System.out.println(" ?X? "); };
		} else
		    if (assoc instanceof ASTType ) {
			ASTType tn = (ASTType)assoc;
			try {
			    System.out.println(tn.toStr((Env<EnvEntry>)this));
			}
			catch (Exception _e) { System.out.println(" ?X? ");  };
		    } else
			Pderef(assoc);
	    }
	if (anc!=null) anc.crawl();
    } 

    synchronized static void Pderef(Object assoc)
    {
	if (assoc instanceof IndexedSessionRef)
	    {
		IndexedSessionRef aa = (IndexedSessionRef)assoc;
		System.out.println(aa + " -> "+aa.getSessionRec()+" @ "+aa.getOffset() + " " +
				   aa.getSessionRec().getFrame());

	    }
	else
	    System.out.println(" * "+assoc);
    }
}
