package pt.ulisboa.tecnico.cllsj;

public class SessionRecord {
    int size;
    SessionField slots[];
    SessionClosure cont;  
    public boolean polarity;
    public boolean polarity_dual;

    // manual memory management for session records
  
    static final int MAXS = 17;
    volatile SessionRecord next_free;
    static SessionRecord cache[] = new SessionRecord[MAXS];
    static int statsalloc[] = new int[MAXS];
    static int statsnew[] = new int[MAXS];


    public SessionRecord (int _size) {
	size = _size;
	slots = new SessionField[_size];
	cont = new SessionClosure();
	next_free = null;
	polarity = true;
    }

    // manual memory management for session records

    static void clearStats()
    {
	for (int i=1; i<MAXS; i++) {
	    statsalloc[i] = statsnew[i] = 0;
	    cache[i] = null;
	}
    }
    
    static void dumpStats()
    {
	for (int i=0; i<MAXS; i++) {
	    System.out.printf("%8d: %8d (%8d)\n",i,statsalloc[i],statsnew[i]);
	}
    }
	
    static  synchronized
	SessionRecord newSessionRecord(int _size)
    {
	statsnew[_size]++;
	if (cache[_size]==null)  {
	    statsalloc[_size]++;
	    // System.out.println("alloc "+_size);
	    // if (_size == 14)  { _size = _size / 0; }
	    return new SessionRecord(_size);
	} else  {
	    SessionRecord sr = cache[_size];
	    cache[_size] = sr.next_free;
	    sr.next_free = null;
	    sr.polarity = true;
	    //sr.step = false;
	    return sr;
	}
    }

    static  synchronized
	void freeSessionRecord(SessionRecord sr)
    {
	int sz = sr.getSize();
	// System.out.println("free "+sz);
	
	sr.next_free = cache[sz];
	cache[sz]=sr;
	
    }

    void setcch(String ch)
    {
	cont.setId(ch);
    }

    String getcch()
    {
	return cont.getId();
    }

    public void dump(int l)
    {
	System.out.println("dump "+this);
	for (int i = 0; i<l; i++) {
	    System.out.println(">> "+slots[i]);
	}
    }
    
    public void writeSlot(SessionField _f, int i)
    {

	/*
	if (slots[i]!=null && _f != null) {
	    System.out.println(">> "+slots[i]);
	    System.exit(0);
	}
	System.out.println("write "+slots+ " "+ i + " := "+_f);
	*/
	slots[i] = _f;
    }
    
    public SessionField readSlot( int i)
    {
	//	System.out.println("read "+this+ " ["+i+"]");
	return slots[i];
    }

    public void setPol(boolean _pol)
    {
	polarity = _pol;
    }

    public boolean getPol()
    {
	return polarity;
    }
   
    public void setPolDual(boolean _pol)
    {
	polarity_dual = _pol;
    }

    public boolean getPolDual()
    {
	return polarity_dual;
    }
    
    public void setCont(ASTNode _cont)
    {
	cont.setBody(_cont);
    }


    public ASTNode getCont()
    {
	return cont.getBody();
    }

    
    public Env<SessionField> getFrame()
    {
	return cont.getEnv();
    }

     public void setFrame( Env<SessionField> _frame)
    {
	cont.setEnv(_frame);
    }    

    public Env<EnvEntry> getFrameP()
    {
	return cont.getEnvP();
    }

     public void setFrameP( Env<EnvEntry> _ep)
    {
	cont.setEnvP(_ep);
    }    
    
    public int getSize()
    {
	return size;
    }
   
}
