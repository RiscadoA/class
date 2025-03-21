package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTAdd;
import pt.inescid.cllsj.ast.nodes.ASTBang;
import pt.inescid.cllsj.ast.nodes.ASTBool;
import pt.inescid.cllsj.ast.nodes.ASTCall;
import pt.inescid.cllsj.ast.nodes.ASTCase;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCoExpr;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTDiv;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTExpr;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTId;
import pt.inescid.cllsj.ast.nodes.ASTInt;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTMul;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTPrintLn;
import pt.inescid.cllsj.ast.nodes.ASTProcDef;
import pt.inescid.cllsj.ast.nodes.ASTProgram;
import pt.inescid.cllsj.ast.nodes.ASTPromoCoExpr;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSelect;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.nodes.ASTString;
import pt.inescid.cllsj.ast.nodes.ASTSub;
import pt.inescid.cllsj.ast.nodes.ASTUnfold;
import pt.inescid.cllsj.ast.nodes.ASTVId;
import pt.inescid.cllsj.ast.nodes.ASTWhy;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.expressions.IRAdd;
import pt.inescid.cllsj.compiler.ir.expressions.IRBool;
import pt.inescid.cllsj.compiler.ir.expressions.IRDiv;
import pt.inescid.cllsj.compiler.ir.expressions.IRExpression;
import pt.inescid.cllsj.compiler.ir.expressions.IRInt;
import pt.inescid.cllsj.compiler.ir.expressions.IRMul;
import pt.inescid.cllsj.compiler.ir.expressions.IRString;
import pt.inescid.cllsj.compiler.ir.expressions.IRSub;
import pt.inescid.cllsj.compiler.ir.expressions.IRVar;
import pt.inescid.cllsj.compiler.ir.instructions.IRBranchOnPolarity;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.TypeArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRFreeSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRJump;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRNextTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRPrint;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushExpression;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushTag;
import pt.inescid.cllsj.compiler.ir.instructions.IRReturn;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRGenerator extends ASTNodeVisitor {
  private IRProgram program = new IRProgram();
  private IRProcess process;
  private IRBlock block;
  private Stack<Environment> environments = new Stack<>();
  private Map<String, Environment> processEnvironments = new HashMap<>();

  public static IRProgram generate(Env<EnvEntry> ep, ASTProgram ast) {
    final IRGenerator gen = new IRGenerator();

    for (ASTProcDef procDef : ast.getProcDefs()) {
      gen.processEnvironments.put(procDef.getId(), new Environment(ep, procDef));
    }

    for (ASTProcDef procDef : ast.getProcDefs()) {
      procDef.accept(gen);
    }

    return gen.program;
  }

  // ==================================== AST node visitors =====================================

  private void visit(IRBlock block, ASTNode node) {
    this.block = block;
    node.accept(this);
  }

  private void visit(IRBlock block, Runnable code) {
    this.block = block;
    code.run();
  }

  @Override
  public void visit(ASTNode node) {
    throw new UnsupportedOperationException(
        "Unsupported AST node: " + node.getClass().getSimpleName());
  }

  @Override
  public void visit(ASTProcDef node) {
    Environment env = processEnvironments.get(node.getId());
    process =
        new IRProcess(
            node.hasArguments(),
            env.getSize(),
            env.getTypeVarCount(),
            countEndPoints(node.getRhs()));
    program.addProcess(node.getId(), process);
    environments.push(env);
    visit(process.getEntry(), node.getRhs());
    environments.pop();
  }

  @Override
  public void visit(ASTCut node) {
    IRBlock lhs = process.addBlock("cut_lhs");
    IRBlock rhs = process.addBlock("cut_rhs");
    IRType type = ASTIntoIRType.convert(environment(), node.getChType());

    // Choose the initial block based on the polarity of the channel type.
    branchOnPolarity(
        node.getChType(),
        () -> {
          block.add(new IRNewSession(record(node.getCh()), type, lhs.getLabel()));
          block.add(new IRJump(rhs.getLabel()));
        },
        () -> {
          block.add(new IRNewSession(record(node.getCh()), type, rhs.getLabel()));
          block.add(new IRJump(lhs.getLabel()));
        });

    visit(lhs, node.getLhs());
    visit(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTMix node) {
    IRBlock lhs = process.addBlock("mix_lhs");
    IRBlock rhs = process.addBlock("mix_rhs");

    block.add(new IRNewTask(rhs.getLabel()));
    block.add(new IRJump(lhs.getLabel()));

    visit(lhs, node.getLhs());
    visit(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTFwd node) {
    this.branchOnPolarity(
        node.getCh2Type(),
        () -> block.add(new IRForward(record(node.getCh1()), record(node.getCh2()))),
        () -> block.add(new IRForward(record(node.getCh2()), record(node.getCh1()))));
  }

  @Override
  public void visit(ASTEmpty node) {
    block.add(new IRNextTask());
  }

  @Override
  public void visit(ASTClose node) {
    block.add(new IRPushClose(record(node.getCh())));
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTCoClose node) {
    block.add(new IRPopClose(record(node.getCh())));
    block.add(new IRFreeSession(record(node.getCh())));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSend node) {
    IRBlock closure = process.addBlock("send_closure");
    IRBlock rhs = process.addBlock("send_rhs");
    IRType type = ASTIntoIRType.convert(environment(), node.getLhsType());

    block.add(new IRNewSession(record(node.getCho()), type, closure.getLabel()));
    block.add(new IRPushSession(record(node.getChs()), record(node.getCho())));

    // Flip if the remainder of the session type is negative.
    branchOnPolarity(
        node.getRhsType(),
        () -> {
          block.add(new IRJump(rhs.getLabel()));
        },
        () -> {
          block.add(new IRFlip(record(node.getChs())));
          block.add(new IRJump(rhs.getLabel()));
        });

    visit(closure, node.getLhs());
    visit(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTRecv node) {
    block.add(new IRPopSession(record(node.getChr()), record(node.getChi())));

    IRBlock rhs = process.addBlock("recv_rhs");

    // Flip to the received session if it is negative.
    branchOnPolarity(
        node.getChiType(),
        () -> {
          block.add(new IRJump(rhs.getLabel()));
        },
        () -> {
          block.add(new IRFlip(record(node.getChi())));
          block.add(new IRJump(rhs.getLabel()));
        });

    visit(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTSelect node) {
    IRBlock rhs = process.addBlock("select_rhs");

    block.add(new IRPushTag(record(node.getCh()), node.getLabelIndex()));

    // Flip if the remainder of the session type is negative.
    branchOnPolarity(
        node.getRhsType(),
        () -> {
          block.add(new IRJump(rhs.getLabel()));
        },
        () -> {
          block.add(new IRFlip(record(node.getCh())));
          block.add(new IRJump(rhs.getLabel()));
        });

    visit(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTCase node) {
    Map<Integer, IRPopTag.Case> cases = new HashMap<>();
    block.add(new IRPopTag(record(node.getCh()), cases));

    for (int i = 0; i < node.getCaseCount(); ++i) {
      ASTNode caseNode = node.getCase(node.getCaseLabelFromIndex(i));
      // We don't count the root path, as that's already accounted for in the process
      int endPointCount = countEndPoints(caseNode) - 1;

      IRBlock block = process.addBlock("case_" + node.getCaseLabelFromIndex(i).substring(1));
      cases.put(i, new IRPopTag.Case(block.getLabel(), endPointCount));

      visit(block, caseNode);
    }
  }

  @Override
  public void visit(ASTId node) {
    List<LinearArgument> linearArguments = new ArrayList<>();
    List<TypeArgument> typeArguments = new ArrayList<>();

    for (int i = 0; i < node.getPars().size(); ++i) {
      linearArguments.add(new LinearArgument(record(node.getPars().get(i)), i));
    }

    for (int i = 0; i < node.getTPars().size(); ++i) {
      Polarity polarity = polarity(node.getTPars().get(i));
      IRType type = ASTIntoIRType.convert(environment(), node.getTPars().get(i));
      typeArguments.add(new TypeArgument(type, i, polarity.isPositiveOrDual()));
    }

    // TODO: handle exponential arguments

    block.add(new IRCallProcess(node.getId(), linearArguments, typeArguments));
  }

  @Override
  public void visit(ASTPrintLn node) {
    block.add(new IRPrint(expression(node.getExpr()), node.withNewLine()));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCoExpr node) {
    block.add(new IRPushExpression(record(node.getCh()), expression(node.getExpr())));
    block.add(new IRReturn(record(node.getCh())));
  }

  // ======================================== Utilities =========================================

  private static class Polarity {
    private final Optional<String> id;
    private final boolean polarity;

    public Polarity(ASTType type, Env<EnvEntry> ep) {
      boolean dual = false;
      if (type instanceof ASTNotT) {
        type = ((ASTNotT) type).getin();
        assert type instanceof ASTIdT; // Type checker should guarantee this, right?
        dual = true;
      }

      if (type instanceof ASTIdT) {
        try {
          type = type.unfoldType(ep);
        } catch (Exception e) {
          e.printStackTrace(System.err);
          System.exit(1);
        }
      }

      if (type instanceof ASTIdT) {
        id = Optional.of(((ASTIdT) type).getid());
        polarity = dual;
      } else {
        id = Optional.empty();
        polarity = dual ^ type.isPosCatch(ep);
      }
    }

    public boolean isKnown() {
      return id.isEmpty();
    }

    public boolean isPositive() {
      assert isKnown();
      return polarity;
    }

    public boolean isDual() {
      assert !isKnown();
      return polarity;
    }

    public boolean isPositiveOrDual() {
      return polarity;
    }

    public String getTypeId() {
      assert !isKnown();
      return id.get();
    }
  }

  private Polarity polarity(ASTType type) {
    return new Polarity(type, environment().getEp());
  }

  private void branchOnPolarity(ASTType type, Runnable onWrite, Runnable onRead) {
    branchOnPolarity(polarity(type), onWrite, onRead);
  }

  private void branchOnPolarity(Polarity polarity, Runnable onWrite, Runnable onRead) {
    if (!polarity.isKnown()) {
      // If even after unfolding the type is still a variable, we can't know its polarity at compile
      // time.
      // Therefore, we need to fetch its polarity from the environment, and branch accordingly.
      // We need to do it at runtime, as we don't know the type of the variable at compile time.
      IRBlock negBlock = process.addBlock("polarity_neg");
      IRBlock posBlock = process.addBlock("polarity_pos");
      if (polarity.isDual()) {
        block.add(
            new IRBranchOnPolarity(
                type(polarity.getTypeId()), posBlock.getLabel(), negBlock.getLabel()));
      } else {
        block.add(
            new IRBranchOnPolarity(
                type(polarity.getTypeId()), negBlock.getLabel(), posBlock.getLabel()));
      }
      visit(negBlock, onRead);
      visit(posBlock, onWrite);
    } else if (polarity.isPositive()) {
      onWrite.run();
    } else {
      onRead.run();
    }
  }

  private Environment environment() {
    return environments.peek();
  }

  private int record(String ch) {
    return environment().getIndex(ch);
  }

  private int type(String name) {
    if (!environment().getTypeVarIndices().containsKey(name)) {
      throw new RuntimeException("Type variable " + name + " not found in environment");
    }

    return environment().getTypeVarIndex(name);
  }

  private int countEndPoints(ASTNode node) {
    EndPointCounter counter = new EndPointCounter();
    node.accept(counter);
    return counter.count;
  }

  private static class EndPointCounter extends ASTNodeVisitor {
    private int count = 1;

    @Override
    public void visit(ASTNode node) {
      throw new UnsupportedOperationException(
          "Unsupported AST node: " + node.getClass().getSimpleName());
    }

    @Override
    public void visit(ASTCut node) {
      count += 1;
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTMix node) {
      count += 1;
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTEmpty node) {}

    @Override
    public void visit(ASTClose node) {}

    @Override
    public void visit(ASTId node) {}

    @Override
    public void visit(ASTFwd node) {}

    @Override
    public void visit(ASTCoExpr node) {}

    @Override
    public void visit(ASTPromoCoExpr node) {}

    @Override
    public void visit(ASTCoClose node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTSelect node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTCase node) {
      for (int i = 0; i < node.getCaseCount(); ++i) {
        node.getCase(node.getCaseLabelFromIndex(i)).accept(this);
      }
    }

    @Override
    public void visit(ASTSend node) {
      count += 1;
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRecv node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTPrintLn node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTUnfold node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTCall node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTBang node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTWhy node) {
      node.getRhs().accept(this);
    }
  }

  private IRExpression expression(ASTExpr expr) {
    ExpressionGenerator gen = new ExpressionGenerator();
    expr.accept(gen);
    return gen.ir;
  }

  private class ExpressionGenerator extends ASTExprVisitor {
    private IRExpression ir;

    @Override
    public void visit(ASTExpr expr) {
      throw new UnsupportedOperationException(
          "Unsupported AST expression: " + expr.getClass().getSimpleName());
    }

    @Override
    public void visit(ASTInt node) {
      ir = new IRInt(node.getValue());
    }

    @Override
    public void visit(ASTBool node) {
      ir = new IRBool(node.getValue());
    }

    @Override
    public void visit(ASTString node) {
      ir = new IRString(node.getValue());
    }

    @Override
    public void visit(ASTVId expr) {
      ir = new IRVar(record(expr.getCh()), ASTIntoIRType.convert(environment(), expr.getType()));
    }

    @Override
    public void visit(ASTAdd expr) {
      ir = new IRAdd(expression(expr.getLhs()), expression(expr.getRhs()));
    }

    @Override
    public void visit(ASTSub expr) {
      ir = new IRSub(expression(expr.getLhs()), expression(expr.getRhs()));
    }

    @Override
    public void visit(ASTMul expr) {
      ir = new IRMul(expression(expr.getLhs()), expression(expr.getRhs()));
    }

    @Override
    public void visit(ASTDiv expr) {
      ir = new IRDiv(expression(expr.getLhs()), expression(expr.getRhs()));
    }
  }
}
