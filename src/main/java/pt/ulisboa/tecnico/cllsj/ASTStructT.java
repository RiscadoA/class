package pt.ulisboa.tecnico.cllsj;

import java.util.*;

public class ASTStructT extends ASTType {
  HashMap<String, ASTType> cases;
  ASTType rhs;

  public ASTStructT() {
    cases = new HashMap<String, ASTType>();
  }

  public void addCase(String id, ASTType t) throws Exception {
    if (cases.putIfAbsent(id, t) != null) throw new SyntaxError("Duplicate Label in Choice");
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

  public Pair<ASTType, ASTType> step(String lab, boolean affine) throws Exception {

    ASTType tcase = cases.get(lab);
    ASTType ns;

    if (tcase == null) return null;
    else {
      if (cases.size() == 1) {
        ns = rhs;
      } else {
        ASTStructT ns1 = new ASTStructT();
        for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
          String lab1 = is.next();
          if (!lab1.equals(lab)) {
            ns1.addCase(lab1, cases.get(lab1));
          }
        }
        ns1.setrhs(rhs);
        ns = ns1;
        if (affine) {
          ns = new ASTAffineT(ns);
        }
      }
      return new Pair<ASTType, ASTType>(tcase, ns);
    }
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {

    t = t.unfoldType(e);
    //	t = ASTType.unfoldRec(t);

    if (t instanceof ASTStructT) {
      ASTStructT w = (ASTStructT) t;
      if (cases.size() != w.getcases().size()) return false;
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = w.getcases().get(lab1);
        if (t1 == null) return false;
        if (!t1.equalst(cases.get(lab1), e, lit, trail)) return false;
      }
      ;
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
    ;
  }

  public ASTType dual(Env<EnvEntry> e) {
    ASTCoStructT to = new ASTCoStructT();
    try {
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = cases.get(lab1);
        to.addCase(lab1, t1.dual(e));
        to.setrhs(rhs.dual(e));
      }
      ;
    } catch (Exception never) {
      System.out.println("Exception dual ASTStructT");
    }
    return to;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    return this;
  }

  public String toStr(Env<EnvEntry> e) throws Exception {

    String s = "STRUCT {";
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      s = s + " " + lab1 + " : " + t1.toStr(e) + ",";
    }
    ;
    return s + " }";
  }

  public ASTType subst(Env<ASTType> e) {
    ASTStructT ts = new ASTStructT();
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      try {
        ts.addCase(lab1, t1.subst(e));
      } catch (Exception never) {
      }
    }
    ;
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
        // System.out.println(lab1 + " => "+tag+ " "+base);
        if (tag > max) max = tag;
      } catch (Exception never) {
      }
    }
    ;
    return max;
  }
}
