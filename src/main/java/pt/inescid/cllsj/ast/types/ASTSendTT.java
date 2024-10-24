package pt.inescid.cllsj.ast.types;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeEntry;

public class ASTSendTT extends ASTType {

  String id;
  ASTType rhs;

  public ASTSendTT(String _id, ASTType _rhs) {
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
    if (t instanceof ASTSendTT) {
      ASTSendTT w = (ASTSendTT) t;
      ASTType gs = new ASTIdT(ASTType.gensym());
      EnvEntry gse = new TypeEntry(gs);

      // System.out.println("equalst ASTSendTT sym = "+((ASTIdT)gs).getid());

      e = e.assoc(((ASTIdT) gs).getid(), gse);
      e = e.assoc(w.id, gse);
      e = e.assoc(id, gse);
      if (!rhs.unfoldType(e).equalst(w.rhs.unfoldType(e), e, lit, trail)) return false;
      return true;
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    Env<EnvEntry> el = e.assoc(id, new TypeEntry(new ASTIdT(id)));
    rhs.kindcheck(el);
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {
    Env<EnvEntry> eid = e.assoc(id, new TypeEntry(new ASTIdT(id)));
    return new ASTRecvTT(id, rhs.unfoldType(eid).dual(eid));
    // return new ASTRecvTT(id, rhs.dual(e));
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    //	return new ASTSendTT(id,rhs.unfoldType(e.assoc(id,new TypeEntry( new ASTIdT(id)))));
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    return "SENDT " + id + " ;" + rhs.toStr(e.assoc(id, new TypeEntry(new ASTIdT(id))));
  }

  public ASTType subst(Env<ASTType> e) {
    String bvar = ASTType.gensym(); // to avoid capture of id by e
    e = e.assoc(id, new ASTIdT(bvar));
    return new ASTSendTT(bvar, rhs.subst(e));
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception // need to unfold
      {
    // offset = base;
    ep = ep.assoc(id, new TypeEntry(new ASTIdT(id)));
    rhs = rhs.unfoldType(ep);
    return rhs.SetOffsets(base + 1, ep);
  }
}
