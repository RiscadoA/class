package pt.inescid.cllsj.ast.nodes;

import java.util.List;
import java.util.Set;
import java.util.function.Function;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.types.ASTType;

public class ASTProgramWithIncludes extends ASTNode {
  private List<ASTInclude> incs;
  private List<ASTDList> dLists;
  private List<ASTPList> pLists;

  public ASTProgramWithIncludes(
      List<ASTInclude> incs, List<ASTDList> dLists, List<ASTPList> pLists) {
    this.incs = incs;
    this.dLists = dLists;
    this.pLists = pLists;
  }

  public List<ASTInclude> getIncs() {
    return incs;
  }

  public List<ASTDList> getDLists() {
    return dLists;
  }

  public List<ASTPList> getPLists() {
    return pLists;
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
    throw new Exception("Unexpected call: typecheck");
  }

  public Env<EnvEntry> define(Env<EnvEntry> ep) throws Exception {
    throw new Exception("Unexpected call: define");
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
}
