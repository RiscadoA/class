public class PolarizedSessionRef extends SessionField {

    boolean pol;
    SessionRecord srec;

    PolarizedSessionRef(boolean _pol, SessionRecord _srec)
    {
	pol = _pol;
	srec = _srec;
    }

    public boolean getPol()
    {
	return pol;
    }
    
    public SessionRecord getSessionRec()
    {
	return srec;
    }

}
