package pt.ulisboa.tecnico.cllsj;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

public class ASTEmpty extends ASTNode {

  public ASTEmpty() {}

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void ASTInsertUse(String ch, ASTType _t, ASTNode here, Boolean disCont) throws Exception {
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

  public ASTNode ASTweakeningOnLeaf(String _ch, ASTType t, boolean exp) throws Exception {
    if (exp) {
      ASTNode push = new ASTWhy(_ch, this);
      this.setanc(push);
      return push;
    } else {
      ASTNode push = new ASTDiscard(_ch);
      push.setanc(anc);
      return push;
    }
  }

  public void typecheck(Env<ASTType> ed, Env<ASTType> eg, Env<EnvEntry> ep) {}

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

  public void runproc(Env<EnvEntry> ep, Env<LinSession> ed, Env<Server> eg, Logger logger)
      throws Exception {
    CLLSj.elapsed = System.nanoTime();
  }

  public void sam(Env<SessionField> frame, Env<EnvEntry> ep) throws Exception {
    CLLSj.elapsed = System.nanoTime();
  }

  public void samL(Env<SessionField> frame, Env<EnvEntry> ep, SAMCont p_cont) throws Exception {
    CLLSj.elapsed = System.nanoTime();

    if (CLLSj.trace) {
      System.out.println("empty-op");
    }
    p_cont.code = null;
  }
}
