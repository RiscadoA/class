package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.CLLSj;
import pt.inescid.cllsj.Channel;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.IndexedSessionRef;
import pt.inescid.cllsj.LabelSet;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.LinSessionValue;
import pt.inescid.cllsj.SAMCont;
import pt.inescid.cllsj.SAMError;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.SessionRecord;
import pt.inescid.cllsj.SyntaxError;
import pt.inescid.cllsj.TypeError;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTOfferT;
import pt.inescid.cllsj.ast.types.ASTType;

// unfold

public class ASTCase extends ASTNode {
  String ch;
  TreeMap<String, ASTNode> cases;
  HashMap<String, ASTType> casetypes;
  ASTOfferT offerType;

  public ASTCase(String id) {
    ch = id;
    cases =
        new TreeMap<
            String,
            ASTNode>(); // We need to keep the order of the cases so that we have consistent case
    // indexes
    casetypes = new HashMap<String, ASTType>();
  }

  public void addCase(String id, ASTNode t) throws Exception {
    if (cases.putIfAbsent(id, t) != null) throw new SyntaxError("Duplicate Label in CASE");
  }

  @Override
  public String getSubjectCh() {
    return ch;
  }

  public String getCh() {
    return ch;
  }

  public void setCh(String ch) {
    this.ch = ch;
  }

  public int getCaseCount() {
    return cases.size();
  }

  public String getCaseLabelFromIndex(int index) {
    for (String label : cases.keySet()) {
      if (index == 0) {
        return label;
      }
      index--;
    }

    return null;
  }

  public ASTNode getCase(String label) {
    return cases.get(label);
  }

  public ASTType getCaseType(String label) {
    return casetypes.get(label);
  }

  public Map<String, ASTNode> getCases() {
    return cases;
  }

  public ASTOfferT getOfferType() {
    return offerType;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
      String lab = itCases.next();
      ASTNode cp = cases.get(lab);
      if (cp == from) {
        ASTNode nnode = f.apply(from);
        cp.setanc(nnode);
        cases.replace(lab, nnode);
        nnode.setanc(this);
        break;
      }
    }
  }

  public void ASTInsertUse(String _ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {

    for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
      String lab = itCases.next();
      ASTNode cp = cases.get(lab);
      if (cp == here) {
        ASTUse pushUse = new ASTUse(_ch, here);
        pushUse.setrhs(t);
        pushUse.eg = eg;
        here.setanc(pushUse);
        pushUse.setanc(this);
        cases.replace(lab, pushUse);
        break;
      }
    }
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
      String lab = itCases.next();
      ASTNode cp = cases.get(lab);
      if (cp == caller) {
        cases.replace(lab, newCont);
        break;
      }
    }
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
      String lab = itCases.next();
      ASTNode cp = cases.get(lab);
      if (cp == here) {
        ASTNode pushCall = new ASTCall(ch, cho, t, here);
        pushCall.eg = eg;
        here.setanc(pushCall);
        pushCall.setanc(this);
        cases.replace(lab, pushCall);
        break;
      }
    }
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    if (_ch.equals(ch)) {
      for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
        String lab = itCases.next();
        ASTNode cp = cases.get(lab);
        if (cp == here) {
          ASTNode pushWhy = new ASTWhy(ch, _t, here);
          pushWhy.eg = eg;
          here.setanc(pushWhy);
          pushWhy.setanc(this);
          cases.replace(lab, pushWhy);
          eg.insert(_ch, _t);
          break;
        }
      }
    } else anc.ASTInsertWhyNot(_ch, _t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception {
    for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
      String lab = itCases.next();
      ASTNode cp = cases.get(lab);
      cases.replace(lab, cp.ASTweakeningOnLeaf(ch, typ, exp));
    }
    return this;
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    this.eg = eg;

    // this.inferUses(ch,ed,ep);

    ASTType ty = ed.find(ch);

    ty = ty.unfoldType(ep);
    ty = ASTType.unfoldRecInfer(ty, this, ch, ep);
    if (ty instanceof ASTOfferT) {
      offerType = (ASTOfferT) ty;
      Map<String, ASTType> tcase = offerType.getcases();

      for (Iterator<String> itCasesTyp = tcase.keySet().iterator(); itCasesTyp.hasNext(); ) {
        String lab = itCasesTyp.next();
        if (!cases.containsKey(lab))
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "case: "
                  + lab
                  + " of the OFFER  type: "
                  + ty.toStr(ep)
                  + " not found.");
      }

      for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
        String lab = itCases.next();
        if (!tcase.containsKey(lab))
          throw new TypeError(
              "Line "
                  + lineno
                  + " :"
                  + "case: "
                  + lab
                  + " is not found in the offer type: "
                  + ty.toStr(ep));
      }

      Env<ASTType> eb = ed.dup();

      Env<ASTType> last = null;

      for (Iterator<String> is = tcase.keySet().iterator(); is.hasNext(); ) {
        String lab = is.next();
        ASTType t1 = tcase.get(lab).unfoldType(ep);

        casetypes.put(lab, t1);

        // System.out.println("TC CASE "+lab);

        ASTNode p1 = cases.get(lab);
        ed.upd(ch, t1);
        // System.out.println("ED before TC "); ed.crawl();

        Env<ASTType> egg = eg.assoc("$DUMMY", new ASTBotT());
        p1.typecheck(ed, egg, ep);

        // System.out.println("ED after TC"); ed.crawl();
        ed.updmove(eb);
        // System.out.println("ED "); ed.crawl();
        // System.out.println("EB "); eb.crawl();

        // System.out.println("GO TO LINCLOSE "+lab);
        this.linclose(ed, ep);

        if (last != null && !last.eq(ed))
          throw new TypeError(
              "Line " + lineno + " :" + "OFFER " + ch + ": unbalanced linear contexts");
        last = ed;
        if (is.hasNext()) {
          ed = eb.dup();
          // eg = egg.dupe();
        }
      }
    } else
      throw new TypeError(
          "Line "
              + lineno
              + " :"
              + "case: "
              + ch
              + " is not of OFFER type: "
              + ty.toStr(ep)
              + "found.");
  }

  public Set<String> fn(Set<String> s) {
    s.add(ch);
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab = is.next();
      ASTNode p1 = cases.get(lab);
      s = p1.fn(s);
    }
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    s.add(ch);
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab = is.next();
      ASTNode p1 = cases.get(lab);
      s = p1.fnLinear(s);
    }
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    ASTCase ts = new ASTCase(ch);
    for (Iterator<String> is = cases.keySet().iterator(); is.hasNext(); ) {
      String lab1 = is.next();
      ASTNode t1 = cases.get(lab1);
      try {
        ASTNode t11 = t1.subst(e);
        t11.setanc(ts);
        ts.addCase(lab1, t11);
      } catch (Exception never) {
      }
    }
    return ts;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
    if (y == ch) ch = x;

    for (Iterator<ASTNode> is = cases.values().iterator(); is.hasNext(); ) {
      ASTNode p = is.next();
      p.subs(x, y);
    }
  }

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    Channel channel = (Channel) ed.find(ch);
    // System.out.println("[RUNSTATUS] Start CASE on session "+ session.id);
    String selection = (String) channel.receive();
    // System.out.println("[RUNSTATUS] Finish CASE on session "+ session.id);
    cases.get(selection).runproc(ep, ed, eg, logger);
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {

    SessionField sf = (SessionField) frame.find(ch);

    if (sf instanceof LinSessionValue) {
      LinSessionValue lsv = (LinSessionValue) sf;
      Channel channel = lsv.getLin();
      if (CLLSj.trace) {
        System.out.println("case-op-lc " + ch + " " + channel);
      }
      LabelSet arg = (LabelSet) channel.receive();
      ASTType tyrhs = casetypes.get(arg.getLabel());
      p_cont.code = cases.get(arg.getLabel());
      p_cont.frame = frame;
      p_cont.epnm = ep;
    } else {

      IndexedSessionRef sref = (IndexedSessionRef) sf;
      int doffset = sref.getOffset();
      SessionRecord srec = sref.getSessionRec();
      boolean pol = srec.getPol();

      if (pol) {
        throw new SAMError("use-op + " + ch);
      } else {

        if (CLLSj.trace) {
          System.out.println("case-op " + ch + " " + srec + " @ " + doffset);
        }

        LabelSet arg = (LabelSet) srec.readSlot(doffset);
        srec.writeSlot(null, doffset); // reset linear value!
        sref.incOffset();
        if (arg == null) throw new SAMError("SAM-CASE-read-FAILURE");

        ASTType tyrhs = casetypes.get(arg.getLabel());

        if (tyrhs.isPos(ep)) {
          srec.setPol(true);
          srec.setPolDual(false);
          IndexedSessionRef srefd = (IndexedSessionRef) srec.getFrame().find(srec.getcch());
          sref.resetOffset();
          srefd.resetOffset();
        }

        p_cont.code = cases.get(arg.getLabel());
        p_cont.frame = frame;
        p_cont.epnm = ep;
      }
    }
  }

  public void show() {
    System.out.println(this);
    for (Iterator<String> itCases = cases.keySet().iterator(); itCases.hasNext(); ) {
      String lab = itCases.next();
      ASTNode cp = cases.get(lab);
      System.out.println("** " + lab);
      cp.show();
    }
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
