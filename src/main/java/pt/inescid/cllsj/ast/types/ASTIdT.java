package pt.inescid.cllsj.ast.types;

import java.util.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.KindError;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeDefEntry;
import pt.inescid.cllsj.TypeEntry;

// strategy for lazy unfolding:
// unfold type acts as identity on all types except idT
// on IdT it unfolds using definition, except when .. (closure of co-induction)
//
// ASTId

public class ASTIdT extends ASTType {

  String s;

  ArrayList<ASTType> args;
  boolean haspars = false;

  public ASTIdT(String _s) {
    s = _s;
    args = new ArrayList<ASTType>();
  }

  public void addarg(ASTType ty) {
    args.add(ty);
    haspars = true;
  }

  public String getid() {
    return s;
  }

  boolean selfte(Env<EnvEntry> e) throws Exception {
    /* covers only type entries for now */
    EnvEntry ass;

    // System.out.println("selfte "+s);
    ass = e.find(s);

    // System.out.println("selfte found "+s);

    if (ass instanceof TypeEntry) {
      TypeEntry td = (TypeEntry) ass;
      ASTType t = td.getType();
      if (t instanceof ASTIdT) {
        return (((ASTIdT) t).getid().equals(s));
      } else {
        return false;
      }
    } else {
      TypeDefEntry td0 = (TypeDefEntry) ass;
      ASTType t0 = td0.getTypeDef().getType();
      if (t0 instanceof ASTIdT) {
        if (((ASTIdT) t0).getid().equals(s)) return true;
      }
    }
    return false;
  }

  /*

   if (the,ty) in trail return true

   else

   {

   add (the,ty) to trail

   if (!the.selfte(e)) { the = the.unfoldType(e); return the.equalst(t) }

   else

   if (!t.selfte(e)) { t = t.unfoldType(e); return the.equalst(t) }

   else // the.selfte(e) && t.selfte(e) -- both self (end of chains), the == IdT(s1) && t == IdT(s2)

   if (s1==s2) and arguments equalst the true

   otherwise false

   }

  */

  public boolean equals(Object t) {
    if (t instanceof ASTIdT) {
      return ((ASTIdT) t).getid().equals(s);
    }

    return false;
  }

  public boolean equalst(ASTType t, Env<EnvEntry> e, boolean lit, Trail trail) throws Exception {
    ASTIdT the = this;

    //	if (lit && trail.size()> 3 ) {
    // System.out.println("TRAIL > 3");
    // System.exit(0);
    // }

    // if (lit) trail.dump(e);

    // System.out.println(this +" EQUALS_ID "+t);

    if (!lit) {
      return (t instanceof ASTIdT && ((ASTIdT) t).getid() == s);
    }

    // System.out.println(this.toStr(e)+" EQUALS** "+t.toStr(e));

    if (this == t || this.equals(t)) {
      return true;
    }

    // System.out.println("?? In TRAIL " +s+" - > "+t.toStr(e));

    if (trail.in(s, t)) {
      // System.out.println("FOUND IN TRAIL");
      return true;
    }

    trail.add(s, t);

    // System.out.println("ADDED TO TRAIL "+s+" -> "+t.toStr(e));

    // try {

    if (!the.selfte(e)) {
      ASTType tn = the.unfoldType(e);
      // tn = ASTType.unfoldRec(tn); removed in pass set 2024
      // System.out.println("DEBUG equalst 1 the.unfold = "+tn + " == " + t);
      return tn.equalst(t, e, lit, trail);
    } else if (t instanceof ASTIdT && !((ASTIdT) t).selfte(e)) {
      ASTType tn = t.unfoldType(e);
      // System.out.println("DEBUG equalst 2 the.unfold = "+the + " == " + tn);
      return the.equalst(tn, e, lit, trail);
    } else {

      if (!(t instanceof ASTIdT)) return false;

      // the.selfte(e) && the == IdT(s1) && t == IdT(s2)

      // System.out.println("CHECK X "+the + t); // sometimes t comes ASTNot! BUG to correct.

      ASTIdT w0 = (ASTIdT) the;
      ASTIdT w1 = (ASTIdT) t;

      if (haspars) {
        Iterator<ASTType> ait = w0.args.iterator();
        if (w0.args.size() == args.size()) {
          for (ASTType ty : args) {
            if (!ty.equalst(ait.next(), e, lit, trail)) return false;
          }
        } else System.out.println("ID: # args # parameters mismatch");
      }
      // System.out.println("CMP "+ w0.getid() + " == "+ w1.getid() );
      return (w0.getid().equals(w1.getid()));
    }
    // } catch (Exception _e) { System.out.println("IDT: UNEXPECTED"); }
    // return false;
  }

  public void kindcheck(Env<EnvEntry> e) throws Exception {
    e.find(s);
    if (haspars) {
      for (ASTType ty : args) {
        ty.kindcheck(e);
      }
    }
  }

  boolean selft(ASTType t) {
    if (t instanceof ASTIdT) {
      // System.out.println("SELFT "+((ASTIdT)t).getid()+" "+s);
      if (((ASTIdT) t).getid().equals(s)) return true;
    }
    // else System.out.println("NOT SELFT"+t.toString())
    return false;
  }

  public ASTType unfoldType(Env<EnvEntry> e) throws Exception {

    // System.out.println("UF iDT id="+s);

    EnvEntry ass;

    ass = e.find(s);

    if (ass instanceof TypeEntry) {

      TypeEntry td = (TypeEntry) ass;

      // System.out.println("TypeEntry id="+s+ " -> " + td.getType().toStr(e));

      return td.getType();
    } else if (ass instanceof TypeDefEntry) {

      TypeDefEntry td = (TypeDefEntry) ass;

      ASTType tmptype = td.getTypeDef().getType();

      List<String> pars = td.getTypeDef().args;

      // System.out.println("TypeDefEntry id="+s);

      ASTType tdn = td.getTypeDef().rhs;

      if (pars.size() != args.size()) {
        throw new KindError(s + " parameter and argument list length mismatch");
      }

      Iterator<ASTType> argsi = args.iterator();

      Env<EnvEntry> en = e;

      for (String par : pars) {

        // System.out.println("TypeDefEntry id="+s+" PAR = "+par);

        ASTType argi = argsi.next(); // .unfoldType(e);

        //  System.out.println("TypeDefEntry id="+s+" PAR = "+par+" ARG = "+argi.toStr(e));

        {
          Env<ASTType> te = new Env<ASTType>().assoc(par, argi);
          tdn = tdn.subst(te);
        }
      }

      // System.out.println("After add pars");

      // en.crawl();

      // ASTType tdn = td.getTypeDef().rhs.unfoldType(en); // note: here we must construct new
      // structure

      // System.out.println("Unfolded IdT "+s+"(...) = "+tdn);
      // System.out.println("Unfolded IdT "+s+"(...) = "+tdn.toStr(en));

      return tdn.unfoldType(en); // attention!! LOOP?

    } else throw new KindError(s + "type id is not of the appropriate kind.");
  }

  public ASTType dual(Env<EnvEntry> e) throws Exception {

    return new ASTNotT(this);
  }

  public String toStr(Env<EnvEntry> e) throws Exception {
    EnvEntry ee;
    try {
      ee = e.find(s);
      if (ee instanceof TypeEntry) {
        return s;
      } else if (ee instanceof TypeDefEntry) {
        TypeDefEntry td = (TypeDefEntry) ee;
        List<String> pars = td.getTypeDef().args;
        String outs = "";
        Iterator<ASTType> iargs = args.iterator();
        // e.crawl();
        while (iargs.hasNext()) {
          outs = outs + iargs.next().toStr(e);
          if (iargs.hasNext()) outs = outs + ",";
        }
        return s + "(" + outs + ")";

      } else return s;
    } catch (Exception _e) {
      return "[" + s + "]";
    }
  }

  public ASTType subst(Env<ASTType> e) {
    if (!haspars) {
      try {
        return e.find(s);
      } catch (Exception _e) {
      }
      return this;
    } else {
      Iterator<ASTType> ait = args.iterator();
      ASTIdT newid = new ASTIdT(s);
      for (ASTType tya : args) {
        newid.addarg(tya.subst(e));
      }
      return newid;
    }
  }

  public int SetOffsets(int base, Env<EnvEntry> ep) throws Exception {
    // offset = base+10; // must redo
    return base + 10;
  }
}
