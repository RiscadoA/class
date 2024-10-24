package pt.inescid.cllsj.ast.nodes;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeEntry;

public class ASTRecvTT extends ASTType {
  String id;
  ASTType rhs;

  public ASTRecvTT(String _id, ASTType _rhs) {
    id = _id;
    rhs = _rhs;
  }

  public String getid() {
    return id;
  }

  public ASTType getrhs() {
    return rhs;
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    t = t.unfoldType(e);
    if (t instanceof ASTRecvTT) {
      ASTRecvTT w = (ASTRecvTT) t;
      ASTType gs = new ASTIdT(ASTType.gensym());

      // System.out.println("equalst ASTRecvTT sym = "+((ASTIdT)gs).getid());

      EnvEntry gse = new TypeEntry(gs);

      e = e.assoc(((ASTIdT) gs).getid(), gse);
      e = e.assoc(id, gse);
      e = e.assoc(w.id, gse);

      if (!w.rhs.unfoldType(e).equalst(rhs.unfoldType(e), e, lit, trail)) return false;
      return true;
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    Env<EnvEntry> el = e.assoc(id, new TypeEntry(new ASTIdT(id)));
    rhs.unfoldType(e).kindcheck(el);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    Env<EnvEntry> eid = e.assoc(id, new TypeEntry(new ASTIdT(id)));
    ASTType subs = rhs.unfoldType(eid); // .subst(id, (new ASTIdT(id)));
    ASTType r = new ASTSendTT(id, subs.dual(eid));
    // System.out.println(" ASTRecvTT dual = "+this.toStr(e)+" -> "+r.toStr(e));
    return r;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    /*
    ASTType uf = rhs.unfoldType(e.assoc(id,new TypeEntry(new ASTIdT(id))));
    return new ASTRecvTT(id,uf);
    */
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    e = e.assoc(id, new TypeEntry(new ASTIdT(id)));
    return "RECVT " + id + " ;" + rhs.toStr(e);
  }

  public ASTType subst(Env<ASTType> e) {
    String bvar = ASTType.gensym(); // to avoid capture of id by e
    e = e.assoc(id, new ASTIdT(bvar));
    return new ASTRecvTT(bvar, rhs.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception // need to unfold
      {
    // offset = base;
    ep = ep.assoc(id, new TypeEntry(new ASTIdT(id)));
    rhs = rhs.unfoldType(ep);
    return rhs.SetOffsets(base + 1, ep);
  }
}
