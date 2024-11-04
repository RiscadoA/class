package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTCase;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTExpr;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSelect;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.types.ASTType;

public class Environment {
  private Environment parent = null;
  private Map<String, Integer> indices = new HashMap<>();
  private Map<String, Boolean> polarity = new HashMap<>();
  private Map<String, String> sessionCSize = new HashMap<>();

  public static Environment fromNode(ASTNode node, Environment parent) {
    IndexAssigner assigner = new IndexAssigner(parent);
    node.accept(assigner);
    return assigner.env;
  }

  public static Environment fromNode(ASTNode node) {
    return fromNode(node, null);
  }

  public Environment getParent() {
    return parent;
  }

  public int getSize() {
    return indices.size();
  }

  public boolean isLocal(String session) {
    return indices.containsKey(session);
  }

  public int getIndex(String session) {
    return indices.get(session);
  }

  public boolean getPolarity(String session) {
    if (!polarity.containsKey(session) && parent != null) return parent.getPolarity(session);
    return polarity.get(session);
  }

  public String getSessionCSize(String session) {
    return sessionCSize.get(session);
  }

  public void setPolarity(String session, boolean polarity) {
    this.polarity.put(session, polarity);
  }

  public Environment copy() {
    Environment env = new Environment();
    env.indices = this.indices;
    env.polarity =
        this.polarity.entrySet().stream()
            .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().booleanValue()));
    env.sessionCSize = this.sessionCSize;
    return env;
  }

  public void insert(String session, ASTType cType) {
    this.indices.put(session, indices.size());
    this.polarity.put(session, true);
    this.sessionCSize.put(session, SizeCalculator.calculate(cType));
  }

  // A visitor which simply traverses the AST and assigns an index to each session created in it.
  // Send left-hand-sides and replication right-hand-sides are ignored.
  private static class IndexAssigner extends ASTNodeVisitor {
    public Environment env = new Environment();

    public IndexAssigner(Environment parent) {
      env.parent = parent;
    }

    @Override
    public void visit(ASTNode node) {
      throw new UnsupportedOperationException(
          "Nodes of type "
              + node.getClass().getName()
              + " are not yet supported by Environment.IndexAssigner");
    }

    @Override
    public void visit(ASTExpr node) {}

    @Override
    public void visit(ASTCase node) {
      for (ASTNode branch : node.getCases().values()) {
        branch.accept(this);
      }
    }

    @Override
    public void visit(ASTClose node) {}

    @Override
    public void visit(ASTCoClose node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTCut node) {
      env.insert(node.getCh(), node.getChType());
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTEmpty node) {}

    @Override
    public void visit(ASTFwd node) {}

    @Override
    public void visit(ASTMix node) {
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRecv node) {
      env.insert(node.getChi(), node.getChiType());
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTSelect node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTSend node) {
      node.getRhs().accept(this);
    }
  }
}
