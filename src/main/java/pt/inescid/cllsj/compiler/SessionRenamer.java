package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTBang;
import pt.inescid.cllsj.ast.nodes.ASTCall;
import pt.inescid.cllsj.ast.nodes.ASTCase;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTExpr;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTId;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTPrintLn;
import pt.inescid.cllsj.ast.nodes.ASTProcDef;
import pt.inescid.cllsj.ast.nodes.ASTProgram;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSelect;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.nodes.ASTString;
import pt.inescid.cllsj.ast.nodes.ASTUnfold;
import pt.inescid.cllsj.ast.nodes.ASTVId;
import pt.inescid.cllsj.ast.nodes.ASTWhy;

public class SessionRenamer extends ASTNodeVisitor {
  // How many different occurrences of each session name have been found
  private Map<String, Integer> occurrences = new HashMap<>();
  private Map<String, Stack<Integer>> current = new HashMap<>();

  public static void execute(ASTNode node) {
    node.accept(new SessionRenamer());
  }

  private String sanitize(String session) {
    return session.replace("$", "gen");
  }

  private String introduce(String session) {
    Integer occurences = this.occurrences.compute(session, (k, v) -> v == null ? 1 : v + 1);
    current.compute(
        session,
        (k, v) -> {
          if (v == null) {
            v = new Stack<>();
          }
          v.push(occurences);
          return v;
        });
    return sanitize(session) + "_" + occurences;
  }

  private void leave_scope(String session) {
    Stack<Integer> occurences = this.current.get(session);
    if (occurences == null) {
      throw new IllegalStateException("Session " + session + " was not introduced");
    }
    occurences.pop();
    if (occurences.isEmpty()) {
      this.current.remove(session);
    }
  }

  private String rename(String session) {
    Stack<Integer> occurences = this.current.get(session);
    if (occurences == null || occurences.isEmpty()) {
      throw new IllegalStateException("Session " + session + " was not introduced");
    }
    return sanitize(session) + "_" + occurences.lastElement();
  }

  @Override
  public void visit(ASTNode node) {
    throw new UnsupportedOperationException(
        "Renaming of sessions in nodes of type "
            + node.getClass().getSimpleName()
            + " is not yet supported");
  }

  @Override
  public void visit(ASTBang node) {
    String chi = node.getChi();
    node.setChr(rename(node.getChr()));
    node.setChi(introduce(chi));
    node.getRhs().accept(this);
    leave_scope(chi);
  }

  @Override
  public void visit(ASTCall node) {
    String chi = node.getChi();
    node.setChr(rename(node.getChr()));
    node.setChi(introduce(chi));
    node.getRhs().accept(this);
    leave_scope(chi);
  }

  @Override
  public void visit(ASTCase node) {
    node.setCh(rename(node.getCh()));
    for (ASTNode branch : node.getCases().values()) {
      branch.accept(this);
    }
  }

  @Override
  public void visit(ASTClose node) {
    node.setCh(rename(node.getCh()));
  }

  @Override
  public void visit(ASTCoClose node) {
    node.setCh(rename(node.getCh()));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCut node) {
    String ch = node.getCh();
    node.setCh(introduce(ch));
    node.getLhs().accept(this);
    node.getRhs().accept(this);
    leave_scope(ch);
  }

  @Override
  public void visit(ASTEmpty node) {}

  @Override
  public void visit(ASTFwd node) {
    node.setCh1(rename(node.getCh1()));
    node.setCh2(rename(node.getCh2()));
  }

  @Override
  public void visit(ASTId node) {
    for (ASTExpr expr : node.getExprs()) {
      expr.accept(this);
    }

    for (ASTExpr expr : node.getGExprs()) {
      expr.accept(this);
    }

    for (int i = 0; i < node.getPars().size(); i++) {
      node.getPars().set(i, rename(node.getPars().get(i)));
    }

    for (int i = 0; i < node.getGPars().size(); i++) {
      node.getGPars().set(i, rename(node.getGPars().get(i)));
    }
  }

  @Override
  public void visit(ASTMix node) {
    node.getLhs().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTPrintLn node) {
    node.getExpr().accept(this);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTProcDef node) {
    occurrences.clear();

    List<String> args = node.getArgs();
    for (int i = 0; i < args.size(); i++) {
      args.set(i, introduce(args.get(i)));
    }

    List<String> gargs = node.getGArgs();
    for (int i = 0; i < gargs.size(); i++) {
      gargs.set(i, introduce(gargs.get(i)));
    }

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTProgram node) {
    for (ASTProcDef procDef : node.getProcDefs()) {
      procDef.accept(this);
    }
  }

  @Override
  public void visit(ASTRecv node) {
    String chi = node.getChi();
    node.setChr(rename(node.getChr()));
    node.setChi(introduce(chi));
    node.getRhs().accept(this);
    leave_scope(chi);
  }

  @Override
  public void visit(ASTSelect node) {
    node.setCh(rename(node.getCh()));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSend node) {
    String cho = node.getCho();
    node.setChs(rename(node.getChs()));
    node.setCho(introduce(cho));
    node.getLhs().accept(this);
    node.getRhs().accept(this);
    leave_scope(cho);
  }

  @Override
  public void visit(ASTString node) {}

  @Override
  public void visit(ASTVId node) {
    node.setCh(rename(node.getCh()));
  }

  @Override
  public void visit(ASTUnfold node) {
    node.setCh(rename(node.getCh()));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTWhy node) {
    node.setCh(rename(node.getCh()));
    node.getRhs().accept(this);
  }
}
