package pt.inescid.cllsj.ast.types;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.SyntaxError;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeError;

public class ASTCoMutT extends ASTType {
  HashMap<String, ASTType> cases;
  String lockedlab;

  public ASTCoMutT() {
    cases = new HashMap<String, ASTType>();
    lockedlab = null;
  }

  public ASTCoMutT(String _lockedlab, HashMap<String, ASTType> map) {
    this.cases = map;
    lockedlab = _lockedlab;
  }

  public void addCase(String id, ASTType t) throws Exception {
    if (cases.putIfAbsent(id, t) != null)
      throw new SyntaxError("Duplicate Label " + id + " in offer type");
  }

  HashMap<String, ASTType> getcases() {
    return cases;
  }

  public ASTType gettype(String lab) throws Exception {
    ASTType it = cases.get(lab);
    if (it != null) return it;
    else throw new TypeError("Undeclared Label " + lab + " in comut type.");
  }

  public boolean locked() {
    return (lockedlab != null);
  }

  public ASTCoMutT locki(String lab) throws Exception {
    return new ASTCoMutT(lab, cases);
  }

  public void unlocki() throws Exception {
    lockedlab = null;
  }

  public boolean compat(ASTCoMutT o) {
    return (((lockedlab == null) && !o.locked()) || lockedlab.equals(o.lockedlab));
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    t = t.unfoldType(e);
    // t = ASTType.unfoldRec(t);
    if (t instanceof ASTCoMutT) {
      ASTCoMutT w = (ASTCoMutT) t;
      if (!compat(w)) return false;
      if (cases.size() != w.getcases().size()) return false;
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = w.getcases().get(lab1);
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
    ASTMutT to = new ASTMutT();
    try {
      for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
        String lab1 = is.next();
        ASTType t1 = cases.get(lab1);
        to.addCase(lab1, t1.dual(e));
      }
    } catch (Exception never) {
    }
    to.lockedlab = lockedlab;
    return to;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {
    /*	ASTCoMutT to = new ASTCoMutT();
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
    String s = "comut {";
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTType t1 = cases.get(lab1);
      if (lockedlab != null) if (lab1.equals(lockedlab)) lab1 = lab1 + "*";
      s = s + " " + lab1 + " : " + t1.toStr(e);
    }
    return s + " }";
  }

  public ASTType subst(Env<ASTType> e) {
    ASTCoMutT ts = new ASTCoMutT();
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
    ts.lockedlab = lockedlab;
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
