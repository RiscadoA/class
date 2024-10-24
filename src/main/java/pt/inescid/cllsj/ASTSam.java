package pt.inescid.cllsj;

import java.util.*;
import java.util.function.*;

public class ASTSam extends ASTNode {

  ASTNode exp;

  ASTSam(ASTNode _exp) {
    exp = _exp;
  }

  ASTNode getExp() {
    return exp;
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) {}

  public void ASTupdCont(ASTNode newCont, ASTNode caller) throws Exception {}

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType t, boolean exp) throws Exception {
    return null;
  }

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {}

  public void ASTInsertWhyNot(String ch, ASTType t, ASTNode here) throws Exception {}

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {}

  public Set<String> fn(Set<String> s) {
    return s;
  }

  public Set<String> fnLinear(Set<String> s) {
    return s;
  }

  public ASTNode subst(Env<ASTType> e) {
    return this;
  }

  public void subs(String x, String y) { // implements x/y (substitutes y by x)
  }
}
