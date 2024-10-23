package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTCoSSelect extends ASTNode {
  String chr;
  String lab;
  String chi;
  ASTType type;
  ASTType tyrhs;
  ASTNode rhs;

  /*
    p = new ASTCoSSelect(c1.image,t, c3.image, c2.image,p2); // y:T <- x.#l; P
  */

  public ASTCoSSelect(String _chi, ASTType _type, String _chr, String _lab, ASTNode _rhs) {
    chr = _chr;
    chi = _chi;
    lab = _lab;
    type = _type;
    rhs = _rhs;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    if (from == rhs) {
      ASTNode nnode = f.apply(from);
      rhs.setanc(nnode);
      rhs = nnode;
      nnode.setanc(this);
    } else {
      throw new Exception("ASTInsertPipe: call not expected");
    }
  }

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    if (_ch.equals(chr) || _ch.equals(chi)) {
      ASTUse pushUse = new ASTUse(_ch, here);
      pushUse.setrhs(t);
      pushUse.eg = eg;
      here.setanc(pushUse);
      pushUse.setanc(this);
      rhs = pushUse;
    } else anc.ASTInsertUse(_ch, t, this, disCont);
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    rhs = newCont;
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    ASTNode pushCall = new ASTCall(ch, cho, t, here);
    pushCall.eg = eg;
    here.setanc(pushCall);
    pushCall.setanc(this);
    rhs = pushCall;
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType typ, boolean exp) throws Exception {
    if (_ch.equals(chi)) {
      if (!_ch.equals(chr)) { // may place here before receeive
        return this.ASTweakeningHere(_ch, exp);
      }
      throw new TypeError(
          "Line " + lineno + " :" + "for " + _ch + " type pending after RECV on " + chr);
    }
    rhs = rhs.ASTweakeningOnLeaf(_ch, typ, exp);
    return this;
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(chr) || _ch.equals(chi)) {
      ASTNode pushWhy = new ASTWhy(_ch, here);
      here.setanc(pushWhy);
      pushWhy.setanc(this);
      rhs = pushWhy;
      eg.insert(_ch, _t);
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  void addCoAffines(ASTCoStructT ty, int n) throws Exception {
    Function<ASTNode, ASTNode> f = (ASTNode x) -> new ASTUse(chr, x);
    anc.ASTInsertPipe(f, this);
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {

    ASTType typee = null;
    ASTType ty = ed.find(chr);
    int coaffine_count = 0;

    this.eg = eg;

    // this.inferUses(chr,ed,ep);  !! changed because of struct / costruct treatment of affinity

    if (type != null) {
      typee = type.unfoldType(ep);
      typee = ASTType.unfoldRec(typee);
    }
    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, chr, ep);

    while (ty instanceof ASTCoAffineT) {
      ty = ((ASTCoAffineT) ty).getin();
      ty = ty.unfoldType(ep);
      ty = ASTType.unfoldRec(ty);
      coaffine_count++;
    }

    if (ty instanceof ASTCoStructT) {

      ASTCoStructT tyr = (ASTCoStructT) ty;

      System.out.println("COSELECT " + chr + "." + lab);

      if (coaffine_count > 0 && ASTType.affine(tyr.dual(ep), ep)) {
        System.out.println("Add coaffines !");
        addCoAffines(tyr, coaffine_count);
      }

      // offset = tyr.GetOffset(); // for SAM
      // System.out.println("RECV offset "+chr+"@"+offset);
      // System.out.println("RECV "+chi+" types found="+tyr.getlhs().toStr(ep)+"
      // declared="+typee.toStr(ep));

      Pair<ASTType, ASTType> st = tyr.step(lab, (coaffine_count > 0));

      if (st == null)
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "COSTRUCT "
                + chr
                + " type mismatch: label "
                + lab
                + " not found in "
                + tyr.toStr(ep));
      ty = st.fst.unfoldType(ep);
      ty = ASTType.unfoldRec(ty);
      // System.out.println("SND = "+st.snd);
      tyrhs = st.snd.unfoldType(ep);

      type = ty;

      if (typee != null && !typee.equalst(ty, ep, true, new Trail()))
        throw new TypeError(
            "Line "
                + lineno
                + " :"
                + "RECV "
                + chi
                + " type mismatch; found="
                + type.toStr(ep)
                + " declared="
                + typee.toStr(ep));

      ed.upd(chr, tyrhs);
      Env<ASTType> ext = ed.assoc(chi, ty);
      ep = ASTNode.propagateRVar(ep, chr, chi);
      rhs.typecheck(ext, eg, ep);
      rhs.linclose(ed, ep);

      rhs = ASTInferLinClose(rhs, chi, ext, ep);

    } else throw new TypeError("Line " + lineno + " :" + "RECV " + chr + " is not of RECV type.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(chr);
    s = rhs.fn(s);
    s.remove(chi);
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(chr);
    s = rhs.fnLinear(s);
    s.remove(chi);
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTCoSSelect p;
    if (type == null) p = new ASTCoSSelect(chi, type, chr, lab, rhs.subst(e));
    else p = new ASTCoSSelect(chi, type.subst(e), chr, lab, rhs.subst(e));
    p.rhs.setanc(p);
    return p;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == chr) {
      chr = x;
      rhs.subs(x, y);
    } else if (x == chi) { // we rename the bound name chi to fresh to avoid capturing name x
      String fresh = ASTNode.gensym();
      rhs.subs(fresh, chi);
      chi = fresh;
      rhs.subs(x, y);
    } else if (y != chi) rhs.subs(x, y);
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {

    Channel channel = (Channel) ed.find(chr);

    System.out.println("+COSSELECT: " + lab + "@" + chr);
    LinSession session_in = (LinSession) channel.receive(lab);
    System.out.println("-COSSELECT: " + lab + "@" + chr);

    // System.out.println("[RunStatus] COSELECT on "+session_in.id+" end.");

    rhs.runproc(ep, ed.assoc(chi, session_in), eg, logger);
  }

  public void show() {
    System.out.println(this);
    rhs.show();
  }
}
