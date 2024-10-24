package pt.inescid.cllsj;

import java.util.*;

public class Trail {
  HashMap<String, List<ASTType>> store;
  int S;

  public Trail() {
    store = new HashMap<String, List<ASTType>>();
  }

  boolean in(String x, ASTType t) {
    List<ASTType> ls = store.get(x);
    if (ls == null) return false;
    // System.out.println("TRAIL FOUND "+x+" ->? "+t);
    // for (ASTType tl : ls) System.out.println("** "+x+" "+tl.equals(t));
    // System.out.println("ls.contains(t) = "+ls.contains(t)+"\n\n");
    return ls.contains(t);
  }

  void add(String x, ASTType t) {
    List<ASTType> ls = store.get(x);
    if (ls == null) {
      // System.out.println("new trail bucket for "+x);
      ls = new ArrayList<ASTType>();
      store.put(x, ls);
    } // else  System.out.println("found bucket for "+x)
    ls.add(t);
    // System.out.println("trail added "+x+" -> "+t);
    S++;
  }

  int size() {
    return S;
  }

  void dump(Env<EnvEntry> e) throws Exception {
    for (String key : store.keySet()) {
      for (ASTType t : store.get(key)) System.out.println(key + " - > " + t.toStr(e));
    }
  }
}
