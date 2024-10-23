package pt.ulisboa.tecnico.cllsj;

public class IndexedSessionRef extends SessionField {

  int offset;
  SessionRecord srec;

  IndexedSessionRef(int _ofs, SessionRecord _srec) {
    //	System.out.println("IndexedSessionRef("+_ofs+")");
    offset = _ofs;
    srec = _srec;
  }

  public int getOffset() {
    return offset;
  }

  public void resetOffset() {
    offset = 0;
  }

  public void setOffset(int off) {
    offset = off;
  }

  public void incOffset() {
    //	System.out.println("incOffset "+this);
    offset++;
  }

  public SessionRecord getSessionRec() {
    return srec;
  }

  public void setSessionRec(SessionRecord s) {
    srec = s;
  }

  void UpdSessionRefInplace(SessionRecord srec, int offs) {
    this.setOffset(offs);
    this.setSessionRec(srec);
  }
}
