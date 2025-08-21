
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.*;
import java.util.logging.*;
import java.util.*;


class LinSession extends Session implements Cell, Channel {

    private Logger logger;

    private final Lock lock;

    private final Condition condition;

    public boolean message2take, putLock, takeLock, nofwd;

    private Object message;

    private LinSession fwdSession;

    private boolean cell;

    private int usages, fwd;

    private boolean lockCell;

    private  String ch;

    private String chi;

    private  ASTNode rhs;

    private  Env<EnvEntry> ep;

    private  Env<Session> ed;

    private Env<Server> eg;

    private static int n = 0;

    private boolean linear; 
    public boolean statecell; 


    LinSession() {
        message2take = false;
        putLock = false;
        takeLock = false;
        cell = false;
        fwdSession = null;
        usages = 1;
        lock = new ReentrantLock();
        condition = lock.newCondition();
    }

    LinSession(String _id) {
        this();
        id = _id + "$" + n++;

    }

    public String getId(){
        return id;
    }

    static void PANIC(String e) {
	System.out.println("PANIC: "+e); // copy to
	System.exit(0);
    }
    
    class LabelCell {

	Object _message;
	boolean waitr;
	String label;
	private final Lock lock;
	private final Condition condition;

	public Object obj() { return _message; }
	
	public LabelCell(String lab)
	{
	    label = lab;
	    lock = new ReentrantLock();
	    condition = lock.newCondition();
	    waitr = true;
	}
	
	public void put(Object _m)
	{
	    lock.lock();
	    _message = _m;
	    //System.out.println ("WROTE "+label+" ");
	    condition.signalAll();
	    lock.unlock();
	    
	}

	public Object get() throws Exception
	{   Object _m;
	    
	    lock.lock();
	    // System.out.println ("GOTREQ "+label);
	    while (_message == null && waitr ) {
		condition.await();
		//System.out.println ("AWAKE read "+label);
	    }
	    if (_message == null) PANIC("assert _message == null");
	    _m =_message;
	    _message = null;
	    //System.out.println ("GOT "+label+" "+_message);
	    waitr = true;
	    condition.signalAll();
	    lock.unlock();
	    return _m;
	    
	}

	public void merge(Object c) {
	    // assert _message == null;
	    lock.lock();
	    if (_message != null) PANIC("assert _message == null");
	    _message = c;
	    waitr = false;
	    //System.out.println("AWAKE "+c); // copy to
	    condition.signalAll();
	    lock.unlock();
	}
	    
    }

    public HashMap<String,LabelCell> cells = new HashMap<String,LabelCell>();
    
    private LabelCell getorallocLC(String lab) {
	LabelCell c = cells.get(lab);
	if (c==null) {
	    c = new LabelCell(lab);
	    cells.put(lab,c);
	}
	return c;
    }

    public String toStrQueue() {

	String q ="";
	for (Map.Entry<String, LabelCell> set : cells.entrySet()) {
            q = q + set.getKey()+"("+ set.getValue().obj()+ ") \n";
        }
	return q+"\n";
    }
    
    public void send(String lab,Object _message) throws Exception
    {
	System.out.println("+SENDL 0");
	lock.lock();
	System.out.println("+SENDL 0L");

	while(putLock) {
	    System.out.println("SENDL 1");
	    condition.await();
	}
	putLock = true;
	
	if(fwdSession!=null) {
	    //System.out.println("go fwd send");
	    fwdSession.send(lab,_message);
 	    //System.out.println("back fwd send");
	    putLock = true;
            lock.unlock();
	}
	else {
	    LabelCell c = getorallocLC(lab);
	    putLock = false;
	    System.out.println("-SENDL 0");
	    c.put(_message);
	    lock.unlock();
	}
    }
    
	
    public Object receive(String lab) throws Exception{
	Object _message;
	System.out.println("+RECVL 0");
        lock.lock();
	System.out.println("+RECVL 0L");

	while(takeLock) {
	    System.out.println("RECVL 1");
	    condition.await();
	}
	takeLock = true;
	
	if(fwdSession!=null) {
	    //System.out.println("go fwd recv");
	    _message =  fwdSession.receive(lab);
 	    //System.out.println("back fwd recv");
	    takeLock = false;
            lock.unlock();
	    return _message;
    	} else {
	    LabelCell c = getorallocLC(lab);
	    takeLock = false;
	    System.out.println("-RECVL 0");
	    _message = c.get();
	    lock.unlock();
	    return _message;
	}
	
    }

    
    public void send(Object _message) throws Exception {


	lock.lock();
	//System.out.println("wsend -> "+id+" "+_message+" "+this+" cell?="+cell);
	while (putLock) { 
	    condition.signalAll();
	    condition.await();
        }
        putLock = true;
        message = _message;
        message2take = true;
        condition.signalAll();
	while (message2take && (fwdSession == null)) {
            condition.await();  // !message2take || fwdSession != null
	}
	// !message2take || fwdSession != null

	if (!message2take) {
            putLock = false;
            condition.signalAll();
	    lock.unlock();
	    return;
        } else {
	    //System.out.println("fwdsend -> "+fwdSession+" "+id+" "+_message+" "+this);
            fwdSession.send(_message);
            message2take = false;
            putLock = false;
            condition.signalAll();
	    lock.unlock();
        }
    }


    public Object receive() throws Exception {

        lock.lock();
	//System.out.println("wreceive -> "+id+" "+this);

        while(takeLock) {
	    condition.await();
	}
        takeLock = true;
	
        while(!message2take && (fwdSession == null)) {
	    condition.await();
	}

	if(message2take){
            Object _message = message;
            message2take = false;
            takeLock = false;
            condition.signalAll();
            lock.unlock();
            return _message;
        }else {
	    //System.out.println("fwdreceive -> "+id+" "+this);
            Object _message = fwdSession.receive();
            takeLock = false;
            condition.signalAll();
	    lock.unlock();
            return _message;
        }
    }


    public LinSession setFwdSession(LinSession fwdSession) throws Exception {

	lock.lock();       
	if(this.fwdSession == null) {
	    // System.out.println("fwd -> "+this+id+" "+" "+fwdSession+" "+fwdSession.getId());
            fwdSession.incUsages(usages - 1);
            this.fwdSession = fwdSession;
	    condition.signalAll();
            lock.unlock();
	    return fwdSession;
        }else {
	    // System.out.println("ffwd -> "+this+id+" "+" "+fwdSession+" "+fwdSession.getId());
            this.fwdSession = this.fwdSession.setFwdSession(fwdSession); 
            lock.unlock();
	    return fwdSession;
        }
    }

    public void setCell(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Session> _ed,
                        Env<Server> _eg, Logger _logger, boolean _linear, boolean _state){
        lock.lock();
        if(fwdSession == null) {
 	    // System.out.println(usages + " SETCELL "+this+" "+_chi+" "+_state);
            cell = true;
            chi = _chi;
            rhs = _rhs;
            ep = _ep;
            ed = _ed;
            eg = _eg;
            logger = _logger;
            lockCell = false;
            condition.signalAll();
            linear = _linear;
            statecell = _state;
            lock.unlock();
        }else {
            lock.unlock();
            fwdSession.setCell(_chi, _rhs, _ep, _ed, _eg, _logger, _linear, _state);
        }
    }

    public void setEmptyCell(Logger _logger){
        lock.lock();
        if(fwdSession == null){
            logger = _logger;
            cell = true;
            linear = true;
            lockCell = true;
            condition.signalAll();
            lock.unlock();
        }else {
            lock.unlock();
            fwdSession.setEmptyCell(_logger);
        }
    }


    public LinSession take(String _chi) throws Exception {
        //System.out.println("LinSession: start take " + id);
        lock.lock();
	//  System.out.println("LinSession: Lock the object. waiting for cell or fwd " + id);
        while (!cell && fwdSession == null) {
            condition.await();
        }
	// lock.unlock();
        if(cell){
	    //  lock.lock();
	    //  System.out.println("Take: A cell is here. Waiting for the lock. " + id);
            while(lockCell){
                condition.await();
            }
	    //   System.out.println("Take: Got the lock. " + id);
            lockCell = true;
            LinSession session = new LinSession(_chi);
            ASTNode rhs_r = rhs;
            Env<EnvEntry> ep_r = ep;
            Env<Session> ed_r = ed;
            String chi_r = chi;
            Env<Server> eg_r = eg;
            //System.out.println("Take: contents = " +_chi+" "+rhs_r);

            CLLSj.threadPool.submit(
				    new Runnable() {
					public void run() {
					    try {
						// System.out.println("TAKE SPAWN "+session+" "+session.getId());
						rhs_r.runproc(ep_r, ed_r.assoc(chi_r, session), eg_r, logger);
					    } catch (Exception e) {
						e.printStackTrace(System.out);
					    }
					}
				    });
            lock.unlock();
            //System.out.println("READ finish" + id);
            return session;
        }
        else {
            //System.out.println("Fwd the read operation on " + id + " to " + fwdSession.getId());
	    //   System.out.println("Take: fwd to other session. " + id);
            lock.unlock();
	    return fwdSession.take(_chi);
        }
    }

    public void put(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Session> _ed, Env<Server> _eg) throws Exception{
        lock.lock();
	// Thread.sleep(3);
        while (!cell && fwdSession == null) {
            condition.await();
        }
        //lock.unlock();
        if(cell){
	    // lock.lock();
            chi = _chi;
            rhs = _rhs;
            ep = _ep;
            ed = _ed;
            eg = _eg;
            lockCell = false;
            condition.signalAll();
            lock.unlock();
        }
        else {
	    lock.unlock();
	    fwdSession.put(_chi, _rhs, _ep, _ed, _eg);
        }
    }

    public void free() throws Exception{
        lock.lock();
        while (!cell && fwdSession == null) {
            condition.await();
        }
        if(cell) {
            //System.out.println(usages + " FREE CELL "+this+" "+chi);
            if (fwdSession != null) {
		        System.out.println("fwdSession != null");
		        System.exit(0);
	    }	    
            usages = usages - 1;
            if(usages == 0 && linear) {

		//When all usages are released and the cell is linear, we need to discard its content

		//System.out.println(usages + " FREE CELL "+this);
		
		LinSession session = new LinSession(chi);
        Env<Session> edCut = ed.assoc(chi,session);
		
        if (statecell) {
		    //System.out.println("STATE CELL RELEASE payload");
            CLLSj.threadPool.submit( new Runnable(){
                public void run() {
                    try {
                            rhs.runproc(ep, edCut, eg, logger);
                    } catch (Exception e) {e.printStackTrace(System.out);} 
                }
            });
            (new ASTRelease(chi)).runproc(ep, edCut, eg, logger);    
        } else {
		    //System.out.println("STATE CELL DISCARD payload");
            CLLSj.threadPool.submit( new Runnable(){
			    public void run(){
                    try {
				            rhs.runproc(ep, edCut, eg, logger);
			        } catch (Exception e) {e.printStackTrace(System.out);} 
            }
		    });
		    (new ASTDiscard(chi)).runproc(ep, edCut, eg, logger);
        }
        }
            lock.unlock();
	    ch = "\n";
            return;
        }
        else { //!cell && fwdSession != null
            lock.unlock();
	    // System.out.println("FREE FWD CELL "+this+" -> "+fwdSession);
	    fwdSession.free();
        }
    }

    public LinSession read(String _chi) throws Exception {
        //System.out.println("LinSession: reading cell " + id);
        lock.lock();
        while (!cell && fwdSession == null) {
            condition.await();
        }
        // lock.unlock();
        if(cell){
            //  lock.lock();
            //System.out.println("Read: A cell is here. " + id);
            LinSession session = new LinSession(_chi);
            ASTNode rhs_r = rhs;
            Env<EnvEntry> ep_r = ep;
            Env<Server> eg_r = eg;
            String chi_r = chi;
            CLLSj.threadPool.submit(
				    new Runnable() {
					public void run() {
					    try {
						rhs_r.runproc(ep_r, new Env<Session>(chi_r, session, null), eg_r, logger);
					    } catch (Exception e) {
						e.printStackTrace(System.out);
					    }
					}
				    });
            lock.unlock();
            //System.out.println("READ finish" + id);
            return session;
        }
        else {
            //System.out.println("Fwd the read operation on " + id + " to " + fwdSession.getId());
            lock.unlock();
            return fwdSession.read(_chi);
        }
    }

    public void write(String _chi, ASTNode _rhs, Env<EnvEntry> _ep, Env<Server> _eg) throws Exception{
        lock.lock();
        while (!cell && fwdSession == null) {
            condition.await();
        }
        //lock.unlock();
        if(cell){
            // lock.lock();
            chi = _chi;
            rhs = _rhs;
            ep = _ep;
            eg = _eg;
            lock.unlock();
        }
        else {
            lock.unlock();
            fwdSession.write(_chi, _rhs, _ep, _eg);
        }
    }

    public void lock() throws Exception{
        //System.out.println("Trying to lock cell " + id);
        lock.lock();
        while (!cell && fwdSession == null) {
            condition.await();
        }
        //  lock.unlock();

        if(cell){
            //   lock.lock();
            while(lockCell){
                condition.await();
            }
            lockCell = true;
            lock.unlock();
        }
        else {
            //System.out.println("Fwd the lock on " + id + " to " + fwdSession.getId());
            lock.unlock();
            fwdSession.lock();
        }
    }

    public void unlock() throws Exception{
        lock.lock();
        while (!cell && fwdSession == null) {
            condition.await();
        }
        // lock.unlock();
        if(cell){
            //  lock.lock();
            lockCell = false;
            condition.signalAll();
            lock.unlock();
        }
        else {
            lock.unlock();
            fwdSession.unlock();
        }
    }


    public void incUsages(int n){
        lock.lock();
        if(fwdSession != null) {
            lock.unlock();
            fwdSession.incUsages(n);
        }
        else {
            usages = usages + n;
            lock.unlock();
        }
    }
}

