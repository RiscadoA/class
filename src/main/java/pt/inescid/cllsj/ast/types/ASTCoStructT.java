package pt.inescid.cllsj.ast.types;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.Pair;
import pt.inescid.cllsj.SyntaxError;
import pt.inescid.cllsj.Trail;

public class ASTCoStructT extends ASTType {
  HashMap<String, ASTType> cases;
  ASTType rhs;

  public ASTCoStructT() {
    cases = new HashMap<String, ASTType>();
  }

  public void addCase(String id, ASTType t) throws Exception {
    if (cases.putIfAbsent(id, t) != null)
      throw new SyntaxError("Duplicate Label " + id + " in offer type");
  }

  HashMap<String, ASTType> getcases() {
    return cases;
  }

  public void setrhs(ASTType r) {
    rhs = r;
  }

  public ASTType getrhs() {
    return rhs;
  }

  public Pair<ASTType, ASTType> step(String lab, boolean coaffine) throws Exception {

    ASTType tcase = cases.get(lab);
    ASTType ns;

    System.out.println("COSTRUCT STEP ");
    if (tcase == null) return null;
    else {
      if (cases.size() == 1) {
        // System.out.println("COSTRUCT LAST "+rhs);
        ns = rhs;
      } else {
        ASTCoStructT ns1 = new ASTCoStructT();
        for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
          String lab1 = is.next();
          if (!lab1.equals(lab)) {
            ns1.addCase(lab1, cases.get(lab1));
          }
        }
        ns1.setrhs(rhs);
        ns = ns1;
        if (coaffine) {
          ns = new ASTCoAffineT(ns);
        }
      }
      return new Pair<ASTType, ASTType>(tcase, ns);
    }
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    t = t.unfoldType(e);
    //	t = ASTType.unfoldRec(t);
    //	System.out.println("OFFER equalst "+this + " "+t);
    if (t instanceof ASTCoStructT) {
      ASTCoStructT w = (ASTCoStructT) t;
      if (cases.size() != w.getcases().size()) return false;
      // System.out.println("OFFER # cases matched");
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = w.getcases().get(lab1);
        if (t1 == null) return false;
        if (!t1.equalst(cases.get(lab1), e, lit, trail)) return false;
      }
      return rhs.equalst(w.getrhs(), e, lit, trail);
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
    ASTStructT to = new ASTStructT();
    try {
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = cases.get(lab1);
        to.addCase(lab1, t1.dual(e));
        to.setrhs(rhs.dual(e));
      }
    } catch (Exception never) {
    }
    return to;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    /*	ASTCoStructT to = new ASTCoStructT();
    for (Iterator<String> is = cases.keySet().iterator();is.hasNext();) {
        String lab = is.next();
        ASTType t = cases.get(lab);
        to.addCase(lab,t.unfoldType(e));
    };
    return to;
    */
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    String s = "costruct {";
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      s = s + "| " + lab1 + " : " + t1.toStr(e);
    }
    return s + " }";
  }

  public ASTType subst(Env<ASTType> e) {
    ASTCoStructT ts = new ASTCoStructT();
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      try {
        ts.addCase(lab1, t1.subst(e));
      } catch (Exception never) {
        System.out.println("SUBST OFFER EX");
        System.exit(0);
      }
    }
    ts.setrhs(rhs.subst(e));
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
    return max;
  }
}
