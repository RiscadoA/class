package pt.inescid.cllsj.ast.types;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.SyntaxError;
import pt.inescid.cllsj.Trail;

public class ASTCaseT extends ASTType {
  HashMap<String, ASTType> cases;

  public ASTCaseT() {
    cases = new HashMap<String, ASTType>();
  }

  public void addCase(String id, ASTType t) throws Exception {
    if (cases.putIfAbsent(id, t) != null) throw new SyntaxError("Duplicate Label in Choice");
  }

  public HashMap<String, ASTType> getcases() {
    return cases;
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {

    //	System.out.println("CASET equalst "+this+" == "+t);

    t = t.unfoldType(e);
    // t = ASTType.unfoldRec(t);

    //	System.out.println("CASETafterunfold equalst "+this.toStr(e)+" == "+t.toStr(e));

    if (t instanceof ASTCaseT) {
      ASTCaseT w = (ASTCaseT) t;
      if (cases.size() != w.getcases().size()) return false;
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = w.getcases().get(lab1);
        //		System.out.println("\nLABEL "+lab1+" "+cases.get(lab1)+" "+t1+"\n");
        if (t1 == null) return false;
        if (!t1.equalst(cases.get(lab1), e, lit, trail)) return false;
      }
      return true;
    }
    return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      t1.kindcheck(e);
    }
  }

  public ASTType dual(Env<EnvEntry> e) {
    ASTOfferT to = new ASTOfferT();
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      try {
        ASTType t1 = cases.get(lab1);
        to.addCase(lab1, t1.dual(e));
      } catch (Exception never) {
        System.out.println("Exception dual ASTCaseT");
      }
    }
    return to;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    /* ASTCaseT to = new ASTCaseT();
    for (Iterator<String> is = cases.keySet().iterator();is.hasNext();) {
        String lab = is.next();
        ASTType t = cases.get(lab);
        to.addCase(lab,t.unfoldType(e));
        };
    */
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {

    String s = "CHOICE OF {";
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      s = s + " | " + lab1 + " : " + t1.toStr(e);
    }
    return s + " }";
  }

  public ASTType subst(Env<ASTType> e) {
    ASTCaseT ts = new ASTCaseT();
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      try {
        ts.addCase(lab1, t1.subst(e));
      } catch (Exception never) {
      }
    }
    return ts;
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) {
    int max = -1;
    // offset = base;
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      try {
        int tag = t1.SetOffsets(base + 1, ep);
        if (tag > max) max = tag;
      } catch (Exception never) {
      }
    }
    //	System.out.println("CASE OFFSETS => " + max);
    return max;
  }
}
