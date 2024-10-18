
public class EnvR<X> { // Runtime environment synchronizer

    Env<X> the_env; 
    
    public EnvR() { the_env = new Env<X>(); }
    
    synchronized public EnvR<X> assoc(String id, X a) {
	the_env = the_env.assoc(id,a);
	return this;
    }
    
    synchronized public void  insert(String id, X a) {
        the_env.insert(id,a);
    }

    synchronized public Env<X> shallowDup(){
	return the_env.shallowDup();
    }

    synchronized public Env<X> dup() {
	return the_env.dup();
    }

    synchronized public Env<X> dupe() {
	return the_env.dupe();
    }

    synchronized public boolean eq(Env<X> e) {
	return the_env.eq(e);
    }

    synchronized public String findI(SessionRecord _it) throws Exception {
	return the_env.findI(_it);
    }		   

    synchronized public X find(String _id) throws Exception {
	return the_env.find(_id);
    }		   

    synchronized public boolean def(String _id) throws Exception {
	return the_env.def(_id);
    }

    synchronized public void upd(String _id, X _assoc) throws Exception {
	 the_env.upd(_id,_assoc);
    }

    synchronized public void updmove(String _id) throws Exception {
	 the_env.updmove(_id);
    }
    /*
    synchronized public void updmove(Env<X> env2) throws Exception {
	return the_env.updmove(env2);
    }
    */
    synchronized public void crawl() {
	 the_env.crawl();
    }
}
