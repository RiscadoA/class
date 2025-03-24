package pt.inescid.cllsj.ast.nodes;

import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.LinSession;
import pt.inescid.cllsj.Server;
import pt.inescid.cllsj.SessionField;
import pt.inescid.cllsj.Value;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.types.ASTType;

public abstract class ASTExpr extends ASTNode {

  public abstract ASTType etypecheck(Env<ASTType> e, Env<ASTType> eg, Env<EnvEntry> ep, boolean lin)
      throws Exception;

  public abstract Value eval(Env<LinSession> ed, Env<Server> eg) throws Exception;

  public void ASTInsertUse(String ch, ASTType t, ASTNode here, Boolean disCont) throws Exception {
    anc.ASTInsertUse(ch, t, this, disCont); // insert above up
  }

  public void ASTInsertWhyNot(String ch, ASTType _t, ASTNode here) throws Exception {
    anc.ASTInsertWhyNot(ch, _t, this);
  }

  public void ASTInsertCall(String ch, String cho, ASTType t, ASTNode here) throws Exception {
    anc.ASTInsertCall(ch, cho, t, this);
  }

  public ASTNode ASTweakeningOnLeaf(String ch, ASTType typ, boolean exp) throws Exception {
    throw new Exception("ASTweakeningOnLeaf: call not expected");
  }

  public Value sameval(Env<SessionField> ed) throws Exception {
    throw new Exception("sameval: call not expected");
  }

  public void ASTInsertPipe(Function<ASTNode, ASTNode> f, ASTNode from) throws Exception {
    throw new Exception("ASTInsertPipe: call not expected");
  }

  public void accept(ASTExprVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void accept(ASTNodeVisitor visitor) {
    visitor.visit(this);
  }
}
