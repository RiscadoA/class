package pt.inescid.cllsj.ast.nodes;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTProgram extends ASTNode {
  private List<ASTDList> dLists = new ArrayList<>();
  private List<ASTPList> pLists = new ArrayList<>();
  private List<ASTTypeDef> typeDefs = new ArrayList<>();
  private List<ASTProcDef> procDefs = new ArrayList<>();

  public ASTProgram(List<ASTDList> typeDefs, List<ASTPList> procDefs) {
    for (ASTDList dList : typeDefs) {
      this.dLists.add(dList);
      this.typeDefs.addAll(dList.getLd());
    }
    for (ASTPList pList : procDefs) {
      this.pLists.add(pList);
      this.procDefs.addAll(pList.getLd());
    }
  }

  public List<ASTTypeDef> getTypeDefs() {
    return typeDefs;
  }

  public List<ASTProcDef> getProcDefs() {
    return procDefs;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    throw new Exception("Unexpected call editASTInsertUse"); // never called
  }

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {
    throw new Exception("Unexpected call.");
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertCall"); // never called
  }

  public void ASTInsertWhyNot(String _ch, ASTType _t, ASTNode here) throws Exception {
    throw new Exception("Unexpected call: ASTInsertWhyNot"); // never called
  }

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    throw new Exception("Unexpected call: ASTweakeningOnLeaf"); // never called
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) throws Exception {
    for (ASTDList dList : dLists) {
      dList.typecheckmany(ed, eg, ep);
      ep = dList.definemany(ed, eg, ep, false);
    }
    for (ASTPList pList : pLists) {
      pList.typecheckmany(ed, eg, ep);
      ep = pList.definemany(ed, eg, ep, false, false);
    }
  }

  public Env<EnvEntry> define(Env<EnvEntry> ep) throws Exception {
    for (ASTDList dList : dLists) {
      ep = dList.definemany(null, null, ep, false);
    }
    for (ASTPList pList : pLists) {
      ep = pList.definemany(null, null, ep, false, false);
    }
    return ep;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
  }

  public Set<String> fn(Set<String> s) {
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
