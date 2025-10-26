package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.ProcEntry;
import pt.inescid.cllsj.RVarEntry;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.Session;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.Trail;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTBangT;
import pt.inescid.cllsj.ast.types.ASTCoRecT;
import pt.inescid.cllsj.ast.types.ASTCointT;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.ast.types.ASTWhyT;

public class ASTId extends ASTNode {

  String id;
  List<ASTExpr> exprs;
  List<ASTExpr> gexprs;

  List<String> pars; // these are generated from exprs, during type-checking
  List<ASTType> parTypes;

  List<String> gpars; // these are generated from gexprs, during type-checking
  List<ASTType> gparTypes;

  List<ASTType> tpars;
  List<ASTType> tparsGen;

  List<String> procTParIds;
  List<ASTType> procParTypes;
  List<ASTType> procGParTypes;

  boolean elaborated = false;

  public ASTId(String _id) {

    id = _id;

    pars = new ArrayList<String>();
    parTypes = new ArrayList<>();
    gpars = new ArrayList<String>();
    gparTypes = new ArrayList<>();
    tpars = new ArrayList<ASTType>();
    exprs = new ArrayList<ASTExpr>();
    gexprs = new ArrayList<ASTExpr>();
  }

  public List<ASTExpr> getExprs() {
    return exprs;
  }

  public List<ASTExpr> getGExprs() {
    return gexprs;
  }

  public List<String> getPars() {
    return pars;
  }

  public List<ASTType> getParTypes() {
    return parTypes;
  }

  public List<String> getGPars() {
    return gpars;
  }

  public List<ASTType> getGParTypes() {
    return gparTypes;
  }

  public List<ASTType> getTPars() {
    return tpars;
  }

  public List<String> getProcTParIds() {
    return procTParIds;
  }

  public List<ASTType> getProcParTypes() {
    return procParTypes;
  }

  public List<ASTType> getProcGParTypes() {
    return procGParTypes;
  }

  public String getId() {
    return id;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    throw new Exception("Unexpected call editASTInsertUse"); // never called
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    if (pars.contains(_ch)) throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
    return this.ASTweakeningHere(_ch, typ, exp);
  }

  public void addpar(String id) {
    pars.add(id);
  }

  public void addExpr(ASTExpr expr) {
    exprs.add(expr);
  }

  public void addGExpr(ASTExpr gexpr) {
    gexprs.add(gexpr);
  }

  public void addTpar(ASTType ty) {
    tpars.add(ty);
  }

  public void addTGpar(ASTType ty) {
    tparsGen.add(ty);
  }

  public void addGpar(String id) {
    gpars.add(id);
  }

  public ASTNode genProcFromExpr(
      String ch,
      ASTExpr expr,
      ASTType formalDual,
      Env<ASTType> ed,
      Env<ASTType> eg,
      Env<EnvEntry> ep)
      throws Exception {
    // compile process from expression
    ASTNode compProc = compileExpr(ch, expr, formalDual, ep);
    // produce cut between compiled process and process id
    ASTCut cutp = new ASTCut(ch, formalDual, this, compProc);
    cutp.eg = this.eg;

    // SAM stuff
    cutp.sessionSize = 1; // enough to store a !A/?A expression value
    // upd ancestor continuation with newly created cut
    this.getanc().ASTupdCont(cutp, this);

    // get ancestors right
    cutp.setanc(this.getanc());
    this.setanc(cutp);
    compProc.setanc(cutp);
    // typecheck compiled process
    Env<ASTType> er = ed.assoc(ch, formalDual);
    compProc.typecheck(er, eg, ep);
    compProc.linclose(er, ep);
    compProc = ASTInferLinClose(compProc, ch, er, ep);
    return cutp;
  }

  public void typecheck2(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    // elaboration phase terminated, now build and type check if call code
    // now type check process with all inferred parts in place
    // all parameters have been generated (no expressions except ASTVId nodes x,y,z)
    // We have something like cut {P |x| cut { ... |z| id(x,y,z,...)}}
    // type inference will normally apply from outermost generated CUT

    EnvEntry tc = ep.find(id);
    ASTProcDef pe = (ASTProcDef) ((ProcEntry) tc).getProc();
    Iterator<ASTType> its = pe.argtypes.iterator();
    Iterator<ASTExpr> itse = exprs.iterator();

    // process linear parameters

    for (String par : pars) {

      System.out.println("linear parameter " + id + " = " + par);

      ASTType formal0 = its.next();
      ASTType formal = formal0.unfoldType(ep);
      ASTType actual = null;

      actual = ed.find(par);
      actual = ASTType.unfoldRecInferParameter(actual, formal, this, par, ep);

      if (!formal.equalst(actual, ep, true, new Trail())) {
        // System.out.println("formal=" + formal.toStr(ep));
        // System.out.println("actual=" + actual.toStr(ep));
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "Type for linear arg "
                + par
                + " of "
                + id
                + "\n "
                + "declared="
                + formal.toStr(ep)
                + "\n found="
                + actual.toStr(ep));

      } else {
        if (!(actual instanceof ASTCointT)) ed.upd(par, null);
      }
      parTypes.add(actual);
    }

    if (pe.rCall) {
      try {
        RVarEntry r = (RVarEntry) ep.find(pars.get(0));
      } catch (Exception e) {
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": recursive call not decreasing, invalid argument "
                + pars.get(0)
                + " found.");
      }
    }

    // process exponential parameters
    Iterator<ASTType> itgs = pe.gargtypes.iterator();
    for (String par : gpars) {
      System.out.println("Unrestricted parameter " + id + " = " + par);
      ASTType formal0 = itgs.next();
      ASTType formal = formal0.unfoldType(ep);
      // check presence in exponential environment
      ASTType actual = null;
      try {
        actual = eg.find(par);
        // check type compatibility below
      } catch (Exception _e_) {
        // check presence in exponential environment
        actual = ed.find(par);
        // System.out.println("formal=" + par + " " + formal.toStr(ep));
        // System.out.println("actual=" + par + " " + actual.toStr(ep));
        actual = actual.unfoldType(ep);
        if (actual instanceof ASTWhyT) {
          ASTWhyT t = (ASTWhyT) actual;
          actual = t.getin();
          // System.out.println(
          //    "ASTInsertWhyNot=" + par + " " + actual.toStr(ep) + " " + this.getanc());
          this.getanc().ASTInsertWhyNot(par, actual, this);
          // System.out.println(
          //    "ASTInsertWhyNotDONE=" + par + " " + actual.toStr(ep) + " " + this.getanc());
          ed.updmove(par);
        }
      }
      // type must match now, otherwise ...
      if (!formal.equalst(actual, ep, true, new Trail())) {
        // System.out.println("formal=" + formal.toStr(ep));
        // System.out.println("actual=" + actual.toStr(ep));
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "Type for unrestricted arg "
                + par
                + " of "
                + id
                + "\n "
                + "declared="
                + formal.toStr(ep)
                + "\n found="
                + actual.toStr(ep));
      }
      ;
      // parTypes.add(actual);
    }
    exprs.clear();
    gexprs.clear();

    /*
    Iterator<ASTType> itgs = pe.gargtypes.iterator();
    for (String par : gpars) {
      ASTType actual;
      ASTType formal = itgs.next().unfoldType(ep);

      try {

        actual = eg.find(par);
        actual = actual.unfoldType(ep);

      } catch (Exception e) {

        actual = ed.find(par);
        actual = actual.unfoldType(ep);

        // System.out.println("formal=" + par + " " + formal.toStr(ep));
        // System.out.println("actual=" + actual.toStr(ep));

        while (!formal.equalst(actual, ep, true, new Trail()) && (actual instanceof ASTCoAffineT)) {

          ASTCoAffineT tyco = (ASTCoAffineT) actual;
          actual = tyco.getin();
          actual = actual.unfoldType(ep);

          ed.upd(par, actual);

          Boolean disposableCont =
              (actual instanceof ASTUsageT)
                  || (actual instanceof ASTCoAffineT)
                  || (actual instanceof ASTWhyT);
          this.getanc().ASTInsertUse(par, actual, this, disposableCont);
        }

        if (actual instanceof ASTWhyT) {
          ASTWhyT t = (ASTWhyT) actual;
          actual = t.getin();
          actual = actual.unfoldType(ep);

          System.out.println(id + " ASTInsertWhyNot=" + par);

          this.getanc().ASTInsertWhyNot(par, actual, this);
          ed.upd(par, null);
        } else
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "ID: "
                  + par
                  + " is neither unrestricted nor does it type linearly with ?");
      }

      if (!formal.equalst(actual, ep, true, new Trail())) {
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "Type for unrestricted arg "
                + par
                + " of "
                + id
                + "\n declared="
                + formal.toStr(ep)
                + "\n found="
                + actual.toStr(ep));
      }
    }
    exprs.clear();
    gexprs.clear();

    */

  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    System.out.println("ID TC =" + id + " line:" + lineno);

    if (elaborated) {
      typecheck2(ed, eg, ep);
    } else { // elaboration phase, expand expressions into processes
      this.eg = eg;
      EnvEntry tc = ep.find(id);

      ASTNode parent = null; // trivial case if no generated

      if (!(tc instanceof ProcEntry))
        throw new TypeError("Line " + lineno + " :" + " " + id + " not a process id.");

      ASTProcDef pe = (ASTProcDef) ((ProcEntry) tc).getProc();

      // match number of type arguments with type parameters

      if (tpars.size() != pe.targs.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": lengths of type argument and parameter list do not match.");

      // kind check type arguments

      for (ASTType targ : tpars) {
        targ.kindcheck(ep);
      }

      tparsGen = new ArrayList<ASTType>();
      procTParIds = new ArrayList<String>();

      // bind type arguments to type parameters

      Iterator<String> itt = pe.targs.iterator();
      Env<EnvEntry> keepIdsEp = ep;
      for (ASTType param : tpars) {
        ASTType actual = param.unfoldType(ep);
        tparsGen.add(actual);
        String formal = itt.next();
        procTParIds.add(formal);
        ep = ep.assoc(formal, new TypeEntry(actual));
        keepIdsEp = ep.assoc(formal, new TypeEntry(new ASTIdT(formal)));
      }

      // match number of linear arguments with linear parameters

      if (exprs.size() != pe.args.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": number of linear arguments and parameters do not match."
                + exprs.size()
                + " "
                + pe.args.size());

      // elaboration phase from argument expressions to linear parameters

      procParTypes = new ArrayList<ASTType>();
      Iterator<ASTType> itargt = pe.argtypes.iterator();

      for (ASTExpr expr : exprs) {

        ASTType formal = itargt.next();
        procParTypes.add(formal.unfoldType(keepIdsEp));
        formal = formal.unfoldType(ep);
        ASTType formalDual = formal.dual(ep);

        if (!(expr instanceof ASTVId)) {
          // generate fresh ch to bind expression "session" value
          String ch = ASTType.gensym();
          // generate and type-check local process from expression
          ASTNode prefix = genProcFromExpr(ch, expr, formalDual, ed, eg, ep);
          if (prefix != null) {
            parent = prefix; // this marks root of inserted cuts
          }
          ;
          // add fresh channel to parameters list
          pars.add(ch);
          // add type of fresh ch to linear type environment
          ed = ed.assoc(ch, formal);
        } else { // expressin is just an id, use the id as it is
          pars.add(((ASTVId) expr).ch);
        }
      }

      // match number of linear arguments with unrestricted parameters

      if (gexprs.size() != pe.gargs.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": number of unrestricted arguments and parameters do not match."
                + exprs.size()
                + " != "
                + pe.args.size());

      // elaboration phase from argument expressions to unrestricted parameters

      procGParTypes = new ArrayList<ASTType>();
      Iterator<ASTType> itgargt = pe.gargtypes.iterator();
      for (ASTExpr gexpr : gexprs) {

        ASTType formal = itgargt.next();
        procGParTypes.add(formal.unfoldType(keepIdsEp));
        formal = formal.unfoldType(ep);
        ASTType formalDual = formal.dual(ep);

        if (!(gexpr instanceof ASTVId)) {
          // generate fresh name ch
          String ch = ASTType.gensym();
          // generate and type-check local process from expression
          ASTNode prefix = genProcFromExpr(ch, gexpr, new ASTBangT(formalDual), ed, eg, ep);
          if (prefix != null) {
            parent = prefix;
          }
          ;
          // add fresh channel to parameters list
          gpars.add(ch);
          // add type ?formal of fresh ch to linear type environment
          ed = ed.assoc(ch, new ASTWhyT(formal));
        } else {
          gpars.add(((ASTVId) gexpr).ch);
        }
        gparTypes.add(formal);
      }

      elaborated = true;
      // parent should be immediate link
      if (parent != null) parent.typecheck(ed, eg, ep);
      else this.typecheck(ed, eg, ep);
    }
  }

  public Set<String> fn(Set<String> s) {
    Iterator<ASTExpr> its = exprs.iterator();
    while (its.hasNext()) s = its.next().fn(s);
    s.addAll(pars);

    Iterator<ASTExpr> itsG = gexprs.iterator();
    while (itsG.hasNext()) s = itsG.next().fn(s);
    s.addAll(gpars);

    return s;
  }

  /*
    public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
      this.eg = eg;
      EnvEntry tc = ep.find(id);

      if (!(tc instanceof ProcEntry))
        throw new TypeError("Line " + lineno + " :" + " " + id + " not a process id.");

      ASTProcDef pe = (ASTProcDef) ((ProcEntry) tc).getProc();

      // match number of type arguments with type parameters

      if (tpars.size() != pe.targs.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": lengths of type argument and parameter list do not match.");

      // kind check type arguments

      for (ASTType targ : tpars) {
        targ.kindcheck(ep);
      }

      tparsGen = new ArrayList<ASTType>();
      procTParIds = new ArrayList<String>();

      // bind type arguments to type parameters

      Iterator<String> itt = pe.targs.iterator();
      Env<EnvEntry> keepIdsEp = ep;
      for (ASTType param : tpars) {
        ASTType actual = param.unfoldType(ep);
        tparsGen.add(actual);
        String formal = itt.next();
        procTParIds.add(formal);
        ep = ep.assoc(formal, new TypeEntry(actual));
        keepIdsEp = ep.assoc(formal, new TypeEntry(new ASTIdT(formal)));
      }

      // match number of linear arguments with linear parameters

      if (exprs.size() != pe.args.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": number of linear arguments and parameters do not match."
                + exprs.size()
                + " "
                + pe.args.size());

      // elaboration phase from argument expressions to linear parameters

      procParTypes = new ArrayList<ASTType>();
      Iterator<ASTType> itargt = pe.argtypes.iterator();

      for (ASTExpr expr : exprs) {
        ASTType formal = itargt.next();
        procParTypes.add(formal.unfoldType(keepIdsEp));
        formal = formal.unfoldType(ep);
        ASTType formalDual = formal.dual(ep);
        if (!(expr instanceof ASTVId)) {
          // generate fresh ch to bind expression "session" value
          String ch = ASTType.gensym();
          // generate process from expression
          genProcFromExpr(ch, expr, formalDual, ed, eg, ep);
          // add fresh channel to parameters list
          pars.add(ch);
          // add type of fresh ch to linear type environment
          ed = ed.assoc(ch, formal);
        } else { // expressin is just an id, use the id as it is
          pars.add(((ASTVId) expr).ch);
        }
      }

      if (gexprs.size() != pe.gargs.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": number of unrestricted arguments and parameters do not match."
                + exprs.size()
                + " != "
                + pe.args.size());

      // elaboration phase from argument expressions to unrestricted parameters

      procGParTypes = new ArrayList<ASTType>();
      Iterator<ASTType> itgargt = pe.gargtypes.iterator();
      for (ASTExpr gexpr : gexprs) {

        ASTType formal = itgargt.next();

        procGParTypes.add(ASTType.unfoldRec(formal.unfoldType(keepIdsEp)));

        formal = formal.unfoldType(ep);
        formal = ASTType.unfoldRec(formal); // comment

        ASTType formalDual = formal.dual(ep);

        // System.out.println("expr=" + gexpr);

        if (!(gexpr instanceof ASTVId)) {
          String ch = ASTType.gensym(); // generate fresh name

          // generate process from expression
          genProcFromExpr(ch, gexpr, new ASTBangT(formalDual), ed, eg, ep);

          // add fresh channel to parameters list
          gpars.add(ch);
          // add type to linear type environment
          ed = ed.assoc(ch, new ASTWhyT(formal));
        } else {
          // System.out.println("pg = " + ((ASTVId) gexpr).ch);
          gpars.add(((ASTVId) gexpr).ch);
        }
        gparTypes.add(formal);
      }

      // System.out.println("TC ID " + id + " S 2");
      //	tpars = ntpars;

      Iterator<ASTType> its = pe.argtypes.iterator();
      Iterator<ASTExpr> itse = exprs.iterator();

      List<String> npars = new ArrayList<String>();

      // process linear parameters

      for (String par : pars) {

        ASTType formal0 = its.next();
        ASTType formal = formal0.unfoldType(ep);

        ASTType formalDual = formal.dual(ep);
        ASTExpr expr = itse.next();

        ASTType pt;

        try {
          pt = ed.find(par);
        } catch (Exception e) {
          pt = eg.find(par);
          String ch = ASTType.gensym();
          genProcFromExpr(ch, expr, formalDual, ed, eg, ep);
          npars.add(ch);
          parTypes.add(pt);
          continue;
        }

        ASTType actual = pt; // .unfoldType(ep);

        actual = ASTType.unfoldRecInferParameter(actual, formal, this, par, ep);

        if (!formal.equalst(actual, ep, true, new Trail())) {
          // System.out.println("formal=" + formal.toStr(ep));
          // System.out.println("actual=" + actual.toStr(ep));

          try {
            String ch = ASTType.gensym();
            genProcFromExpr(ch, expr, formalDual, ed, eg, ep);
            npars.add(ch);
          } catch (Exception e) {
            throw new TypeError(
                "Line "
                    + lineno
                    + " :"
                    + "Type for linear arg "
                    + par
                    + " of "
                    + id
                    + "\n "
                    + "declared="
                    + formal.toStr(ep)
                    + "\n found="
                    + actual.toStr(ep));
          }
        } else {
          // System.out.println("clear " + par + " " + actual.toStr(ep));
          if (!(actual instanceof ASTCointT)) ed.upd(par, null);
          npars.add(par);
        }

        parTypes.add(actual);
      }

      pars = npars;

      if (pe.rCall) {
        try {
          RVarEntry r = (RVarEntry) ep.find(pars.get(0));
        } catch (Exception e) {
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + id
                  + ": recursive call not decreasing, invalid argument "
                  + pars.get(0)
                  + " found.");
        }
      }

      // process exponential parameters

      if (gpars.size() != pe.gargs.size())
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + id
                + ": lengths of unrestricted argument and parameter list do not match.");

      Iterator<ASTType> itgs = pe.gargtypes.iterator();
      for (String par : gpars) {
        ASTType actual;
        ASTType formal = itgs.next().unfoldType(ep);

        try {

          actual = eg.find(par);
          actual = actual.unfoldType(ep);

        } catch (Exception e) {

          actual = ed.find(par);
          actual = actual.unfoldType(ep);

          // System.out.println("formal=" + par + " " + formal.toStr(ep));
          // System.out.println("actual=" + actual.toStr(ep));

          while (!formal.equalst(actual, ep, true, new Trail()) && (actual instanceof ASTCoAffineT)) {

            ASTCoAffineT tyco = (ASTCoAffineT) actual;
            actual = tyco.getin();
            actual = actual.unfoldType(ep);

            ed.upd(par, actual);

            Boolean disposableCont =
                (actual instanceof ASTUsageT)
                    || (actual instanceof ASTCoAffineT)
                    || (actual instanceof ASTWhyT);
            this.getanc().ASTInsertUse(par, actual, this, disposableCont);
          }

          if (actual instanceof ASTWhyT) {
            ASTWhyT t = (ASTWhyT) actual;
            actual = t.getin();
            actual = actual.unfoldType(ep);

            System.out.println(id + " ASTInsertWhyNot=" + par);

            this.getanc().ASTInsertWhyNot(par, actual, this);
            ed.upd(par, null);
          } else
            throw new TypeError(
                "Line "
                    + lineno
                    + " :"
                    + "ID: "
                    + par
                    + " is neither unrestricted nor does it type linearly with ?");
        }

        if (!formal.equalst(actual, ep, true, new Trail())) {
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "Type for unrestricted arg "
                  + par
                  + " of "
                  + id
                  + "\n declared="
                  + formal.toStr(ep)
                  + "\n found="
                  + actual.toStr(ep));
        }
      }
      exprs.clear();
      gexprs.clear();
    }

    public Set<String> fn(Set<String> s) {
      Iterator<ASTExpr> its = exprs.iterator();
      while (its.hasNext()) s = its.next().fn(s);
      s.addAll(pars);

      Iterator<ASTExpr> itsG = gexprs.iterator();
      while (itsG.hasNext()) s = itsG.next().fn(s);
      s.addAll(gpars);

      return s;
    }

  */

  public Set<String> fnLinear(Set<String> s) {
    Iterator<ASTExpr> its = exprs.iterator();
    while (its.hasNext()) s = its.next().fnLinear(s);

    Iterator<ASTExpr> itsG = gexprs.iterator();
    while (itsG.hasNext()) s = itsG.next().fnLinear(s);

    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTId nid = new ASTId(id);
    nid.lineno = this.lineno;
    for (ASTType param : tpars) {
      nid.addTpar(param.subst(e));
    }
    for (String param : pars) {
      nid.addpar(param);
    }
    for (String param : gpars) {
      nid.addGpar(param);
    }
    for (ASTExpr expr : exprs) {
      nid.addExpr(expr);
    }
    for (ASTExpr gexpr : gexprs) {
      nid.addGExpr(gexpr);
    }
    if (tparsGen != null) {
      for (ASTType param : tparsGen) {
        // System.out.println("SUBST ID "+param);
        nid.addTGpar(param.subst(e));
      }
    }
    return nid;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    String s;
    for (Iterator<String> is = pars.iterator(); is.hasNext(); ) {
      s = is.next();
      if (s == y) s = x;
    }

    for (Iterator<String> is = gpars.iterator(); is.hasNext(); ) {
      s = is.next();
      if (s == y) s = x;
    }
  }

  public void runproc(Env<EnvEntry> ep, Env<Session> ed, Env<Server> eg, Logger logger)
      throws Exception {

    EnvEntry tc = ep.find(id);

    if (tc instanceof ProcEntry) {

      ASTProcDef pe = (ASTProcDef) ((ProcEntry) tc).getProc();

      Env<EnvEntry> epDef = pe.getEnv();
      Iterator<String> its = pars.iterator();

      // Env<Session> ned = ed;
      Env<Session> ned = new Env<Session>();
      Env<Server> neg = eg;

      for (String arg : pe.args) {
        String par = its.next();
        Session session = ed.find(par);
        ned = ned.assoc(arg, session);
      }

      Iterator<String> gits = gpars.iterator();
      for (String garg : pe.gargs) {
        String gpar = gits.next();
        Server server = eg.find(gpar);
        neg = neg.assoc(garg, server);
      }

      Iterator<ASTType> tits = tparsGen.iterator();
      // Env<EnvEntry> nep = ep;
      for (String targ : pe.targsGen) {
        ASTType tpar = tits.next().unfoldType(ep);
        epDef = epDef.assoc(targ, new TypeEntry(tpar));
      }
      pe.rhs.runproc(epDef, ned, neg, logger);

    } else throw new TypeError("Line " + lineno + " :" + " " + id + " not a process id.");
  }

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {

    EnvEntry tc = ep.find(id);

    if (tc instanceof ProcEntry) {
      ASTProcDef pe = (ASTProcDef) ((ProcEntry) tc).getProc();
      Env<EnvEntry> epDef = pe.getEnv();

      //	    System.out.println("\nID ="+id);
      //	    epDef.crawl();

      Iterator<String> its = pars.iterator();
      Env<SessionField> newframe = frame;
      Iterator<ASTType> itypes = pe.argtypes.iterator();

      ASTType recAnchor = null;
      String recPar = null;

      for (String arg : pe.args) {
        String par = its.next();
        if (recAnchor == null) {
          recAnchor = itypes.next().unfoldType(ep);
          recPar = par;
        }

        // SAM
        SessionField argpar = frame.find(par);
        newframe = newframe.assoc(arg, argpar);
        /*
          if (argpar instanceof SessionValue) {
          //System.out.println(argpar);
          SessionValue sref = (SessionValue) argpar;
          newframe = newframe.assoc(arg, sref);
          } else {
          IndexedSessionRef sref = (IndexedSessionRef) argpar;
          newframe = newframe.assoc(arg, sref);
          }
        */
      }

      Iterator<String> gits = gpars.iterator();
      for (String garg : pe.gargs) {
        String gpar = gits.next();
        SessionField session = frame.find(gpar);
        newframe = newframe.assoc(garg, session);
      }

      Iterator<ASTType> tits = tparsGen.iterator();
      // Env<EnvEntry> nep = ep;
      for (String targ : pe.targsGen) {
        ASTType titn = tits.next();
        ASTType tpar = titn.unfoldType(ep);

        epDef = epDef.assoc(targ, new TypeEntry(tpar));
      }

      //	    System.out.println("\nID ="+id);
      //	    newframe.crawl();

      if (pe.rec && recAnchor != null && recAnchor instanceof ASTCoRecT) {
        //		System.out.println("\n REC ID ="+id+":"+recAnchor);
        // go positive focus after unfold

        IndexedSessionRef sref = (IndexedSessionRef) frame.find(recPar);
        int doffset = sref.getOffset();
        SessionRecord srec = sref.getSessionRec();

        ASTNode cont = srec.getCont();
        Env<SessionField> frm = srec.getFrame();
        Env<EnvEntry> epn = srec.getFrameP();

        // IndexedSessionRef srefo = (IndexedSessionRef)frm.find(srec.getcch());

        if (false) { // srec.getStep() || cont == null) {
          // System.out.println("go body ...");
          // srec.setStep(false);
          pe.rhs.sam(newframe, epDef);
        } else {
          // System.out.println("recursive call, wait for next rec production ...");
          sref.setOffset(0);
          srec.setPol(true);
          srec.setCont(this);
          srec.setcch(recPar);
          srec.setFrame(newframe);
          srec.setFrameP(epDef);
          cont.sam(frm, epn);
        }
      } else {
        pe.rhs.sam(newframe, epDef);
      }

    } else throw new SAMError("Line " + lineno + " :" + " " + id + " not a process id.");
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    EnvEntry tc = ep.find(id);

    if (tc instanceof ProcEntry) {
      ASTProcDef pe = (ASTProcDef) ((ProcEntry) tc).getProc();
      Env<EnvEntry> epDef = pe.getEnv();

      if (CLLSj.trace) {
        System.out.println("id-op " + id);
      }

      Iterator<String> its = pars.iterator();
      Env<SessionField> newframe = frame;
      Iterator<ASTType> itypes = pe.argtypes.iterator();

      ASTType recAnchor = null;
      // String  recPar = null;
      String recArg = null;

      Iterator<ASTType> tits = tparsGen.iterator();
      // Env<EnvEntry> nep = ep;
      for (String targ : pe.targsGen) {
        ASTType titn = tits.next();
        ASTType tpar = titn.unfoldType(ep);

        epDef = epDef.assoc(targ, new TypeEntry(tpar));
      }

      for (String arg : pe.args) {

        String par = its.next();

        if (recArg == null) recArg = arg;

        if (recAnchor == null) { // just need the first !
          ASTType typearg = itypes.next();
          // System.out.println("par ="+par+" typearg= "+typearg.toStr(epDef));
          recAnchor = typearg.unfoldType(epDef);
          // recPar = par;
        }

        // SAM
        SessionField argpar = frame.find(par);
        newframe = newframe.assoc(arg, argpar);
      }

      Iterator<String> gits = gpars.iterator();
      for (String garg : pe.gargs) {
        String gpar = gits.next();
        SessionField session = frame.find(gpar);
        newframe = newframe.assoc(garg, session);
      }

      p_cont.code = pe.rhs;
      p_cont.frame = newframe;
      p_cont.epnm = epDef;

    } else throw new SAMError("Line " + lineno + " :" + " " + id + " not a process id.");
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
