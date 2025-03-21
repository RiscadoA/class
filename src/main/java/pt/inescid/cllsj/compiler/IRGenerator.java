package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTId;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTProcDef;
import pt.inescid.cllsj.ast.nodes.ASTProgram;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.instructions.IRBranchOnPolarity;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRForward;
import pt.inescid.cllsj.compiler.ir.instructions.IRFreeSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRJump;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRNextTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushClose;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCall.TypeArgument;
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
    process = new IRProcess(node.hasArguments(), env.getSize(), env.getTypeVarCount());
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
    block.add(new IRFlip(record(node.getCh())));
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
    branchOnPolarity(node.getRhsType(),
      () -> {
        block.add(new IRJump(rhs.getLabel()));
      }, () -> {
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
    branchOnPolarity(node.getChiType(),
      () -> {
        block.add(new IRJump(rhs.getLabel()));
      }, () -> {
        block.add(new IRFlip(record(node.getChi())));
        block.add(new IRJump(rhs.getLabel()));
      });

    visit(rhs, node.getRhs());
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

    block.add(new IRCall(node.getId(), linearArguments, typeArguments));
  }

  // ======================================== Utilities =========================================

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
        block.add(new IRBranchOnPolarity(type(polarity.getTypeId()), posBlock.getLabel(), negBlock.getLabel()));
      } else {
        block.add(new IRBranchOnPolarity(type(polarity.getTypeId()), negBlock.getLabel(), posBlock.getLabel()));
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
}
