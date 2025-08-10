package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTExprVisitor;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.TypeArgument;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRVarT;

public class IRGenerator extends ASTNodeVisitor {
  private IRProgram program = new IRProgram();
  private IRProcess process;
  private IRBlock block;
  private Stack<Environment> environments = new Stack<>();
  private Env<EnvEntry> ep;

  public boolean optimizeExponentialExpressionToForward = true;
  public boolean optimizeSendForward = true;
  public boolean optimizeSendValue = true;

  public IRProgram generate(Env<EnvEntry> ep, ASTProgram ast) {
    for (ASTProcDef procDef : ast.getProcDefs()) {
      this.ep = ep;
      for (int i = 0; i < procDef.getTArgs().size(); ++i) {
        TypeEntry entry = new TypeEntry(new ASTIdT(procDef.getTArgsGen().get(i)));
        this.ep = this.ep.assoc(procDef.getTArgs().get(i), entry);
        this.ep = this.ep.assoc(procDef.getTArgsGen().get(i), entry);
      }

      Environment.forEachProcessPolarity(
          this,
          procDef,
          (suffix, env) -> {
            addProcess(procDef.getArgs().size(), procDef.getGArgs().size(), env, procDef.getRhs());
          },
          this.ep);
    }

    return program;
  }

  // ==================================== AST node visitors =====================================

  private void addProcess(
      int linearArgumentCount,
      int exponentialArgumentCount,
      Environment env,
      ASTNode node,
      Consumer<IRBlock> entryConsumer) {
    IRProcess oldProcess = process;
    process =
        new IRProcess(
            linearArgumentCount,
            exponentialArgumentCount,
            env.recordTypes(),
            env.exponentialTypes(),
            env.typeVariablePolarities(),
            countEndPoints(node));
    program.addProcess(env.getName(), process);
    environments.push(env);
    visitBlock(
        process.getEntry(),
        () -> {
          entryConsumer.accept(block);
          node.accept(this);
        });
    environments.pop();
    process = oldProcess;
  }

  private void addProcess(
      int linearArgumentCount, int exponentialArgumentCount, Environment env, ASTNode node) {
    addProcess(linearArgumentCount, exponentialArgumentCount, env, node, block -> {});
  }

  private void visitBlock(IRBlock block, ASTNode node) {
    visitBlock(block, () -> node.accept(this));
  }

  private void visitBlock(IRBlock block, Runnable runnable) {
    IRBlock oldBlock = this.block;
    this.block = block;
    runnable.run();
    this.block = oldBlock;
  }

  @Override
  public void visit(ASTNode node) {
    throw new UnsupportedOperationException(
        "Unsupported AST node: " + node.getClass().getSimpleName());
  }

  @Override
  public void visit(ASTCut node) {
    // Choose the first side to run based on the polarity of the channel type.
    String negLabel;
    ASTNode neg, pos;
    if (isPositive(node.getChType())) {
      negLabel = "cut_lhs";
      neg = node.getLhs();
      pos = node.getRhs();
    } else {
      negLabel = "cut_rhs";
      neg = node.getRhs();
      pos = node.getLhs();
    }

    // If an exponential occurs in both sides, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(pos, name) && nameFreeIn(neg, name)) {
        block.add(new IRIncRefExponential(exponential(name)));
      }
    }

    IRBlock negBlock = process.addBlock(negLabel);
    block.add(new IRNewSession(record(node.getCh()), negBlock.getLabel()));
    pos.accept(this);
    visitBlock(negBlock, neg);
  }

  @Override
  public void visit(ASTMix node) {
    IRBlock rhs = process.addBlock("mix_rhs");

    // If an exponential occurs in both branches, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name)));
      }
    }

    if (node.isConcurrent()) {
      block.add(new IRNewThread(rhs.getLabel()));
    } else {
      block.add(new IRNewTask(rhs.getLabel()));
    }
    node.getLhs().accept(this);

    visitBlock(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTFwd node) {
    if (isPositive(node.getCh2Type())) {
      block.add(new IRForward(record(node.getCh1()), record(node.getCh2())));
    } else {
      block.add(new IRForward(record(node.getCh2()), record(node.getCh1())));
    }
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
    // If an exponential occurs in both sides, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name)));
      }
    }

    int pushedRecord;
    if (optimizeSendForward && node.getLhs() instanceof ASTFwd) {
      ASTFwd fwd = (ASTFwd) node.getLhs();
      String ch = fwd.getCh1().equals(node.getCho()) ? fwd.getCh2() : fwd.getCh1();
      pushedRecord = record(ch);
    } else {
      IRBlock closure = process.addBlock("send_closure");

      block.add(new IRNewSession(record(node.getCho()), closure.getLabel()));
      pushedRecord = record(node.getCho());

      // Flip to the argument if its session type is positive.
      // This execution order is different from the one in the SAM specification, but by computing
      // the values earlier, we benefit more from the exponential pre-computation.
      if (isPositive(node.getLhsType())) {
        flip(record(node.getCho()));
      }

      visitBlock(closure, node.getLhs());
    }

    IRValueRequisites valueRequisites = valueRequisites(node.getLhsType(), false);
    block.add(new IRPushSession(record(node.getChs()), pushedRecord, valueRequisites));

    // Flip if the remainder of the session type is negative.
    flipIfNegative(record(node.getChs()), node.getRhsType());

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRecv node) {
    IRValueRequisites valueRequisites = valueRequisites(node.getChiType(), true);
    block.add(new IRPopSession(record(node.getChr()), record(node.getChi()), valueRequisites));
    node.getRhs().accept(this);

    // We don't flip to the received session here, as if it is negative, the other endpoint has
    // already been executed by the send instruction, as explained in the comment found there.
  }

  @Override
  public void visit(ASTSendTy node) {
    IRBlock closure = process.addBlock("send_ty_closure");

    int previousRecord = record(node.getChs());
    nextRecord(node.getChs());
    int nextRecord = record(node.getChs());

    // Initialize the continuation record, and flip to it if its session type is positive.
    block.add(new IRNewSession(nextRecord, closure.getLabel()));
    flipIfPositive(nextRecord, node.getTypeRhs());
    block.add(new IRPushSession(previousRecord, nextRecord, IRValueRequisites.notValue()));
    block.add(
        new IRPushType(
            previousRecord,
            intoIRType(node.getType()),
            isPositive(node.getType()),
            valueRequisites(node.getType(), false)));
    block.add(new IRReturn(previousRecord));

    // Generate code for the node's continuation.
    visitBlock(closure, node.getRhs());

    previousRecord(node.getChs());
  }

  @Override
  public void visit(ASTRecvTy node) {
    IRBlock positiveBlock = process.addBlock("recv_ty_positive");
    IRBlock negativeBlock = process.addBlock("recv_ty_negative");

    int previousRecord = record(node.getChs());
    nextRecord(node.getChs());
    int nextRecord = record(node.getChs());

    TypeEntry entry = new TypeEntry(new ASTIdT(node.getTyidGen()));
    ep = ep.assoc(node.getTyid(), entry);
    ep = ep.assoc(node.getTyidGen(), entry);
    ep = ep.assoc(node.getTyidPar(), entry);

    int endPoints = countEndPoints(node.getRhs()) - 1;

    block.add(new IRPopSession(previousRecord, nextRecord, IRValueRequisites.notValue()));
    block.add(
        new IRPopType(
            previousRecord,
            type(node.getTyidGen()),
            new IRPopType.Case(positiveBlock.getLabel(), endPoints),
            new IRPopType.Case(negativeBlock.getLabel(), endPoints)));
    positiveBlock.add(new IRFreeSession(previousRecord));
    negativeBlock.add(new IRFreeSession(previousRecord));

    // Generate code for each possible polarity.
    environment().setPolarity(type(node.getTyidGen()), true);
    visitBlock(positiveBlock, node.getRhs());
    environment().setPolarity(type(node.getTyidGen()), false);
    visitBlock(negativeBlock, node.getRhs());

    previousRecord(node.getChs());
  }

  @Override
  public void visit(ASTSelect node) {
    block.add(new IRPushTag(record(node.getCh()), node.getLabelIndex()));

    // Flip if the remainder of the session type is negative.
    flipIfNegative(record(node.getCh()), node.getRhsType());

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCase node) {
    Set<String> exponentials = exponentialNamesFreeIn(node);
    Map<Integer, IRPopTag.Case> cases = new HashMap<>();
    block.add(new IRPopTag(record(node.getCh()), cases));

    for (int i = 0; i < node.getCaseCount(); ++i) {
      String caseLabel = node.getCaseLabelFromIndex(i);
      ASTNode caseNode = node.getCase(caseLabel);
      // We don't count the root path, as that's already accounted for in the process
      int endPointCount = countEndPoints(caseNode) - 1;

      IRBlock caseBlock = process.addBlock("case_" + caseLabel.substring(1));
      cases.put(i, new IRPopTag.Case(caseBlock.getLabel(), endPointCount));

      // Decrement the reference count of any unused exponentials in this case.
      for (String name : exponentials) {
        decExponentialRefIfUnused(caseBlock, caseNode, name);
      }

      visitBlock(caseBlock, caseNode);
    }
  }

  @Override
  public void visit(ASTId node) {
    List<LinearArgument> linearArguments = new ArrayList<>();
    List<ExponentialArgument> exponentialArguments = new ArrayList<>();
    List<TypeArgument> typeArguments = new ArrayList<>();

    for (int i = 0; i < node.getPars().size(); ++i) {
      linearArguments.add(new LinearArgument(record(node.getPars().get(i)), i));
    }

    for (int i = 0; i < node.getGPars().size(); ++i) {
      exponentialArguments.add(new ExponentialArgument(exponential(node.getGPars().get(i)), i));
    }

    for (int i = 0; i < node.getTPars().size(); ++i) {
      typeArguments.add(
          new TypeArgument(
              intoIRType(node.getTPars().get(i)),
              valueRequisites(node.getTPars().get(i), false),
              isPositive(node.getTPars().get(i)),
              i));
    }

    // Determine the suffix to pick the correct process definition based on type argument polarity.
    String suffix = "";
    for (int i = 0; i < node.getTPars().size(); ++i) {
      suffix += isPositive(node.getTPars().get(i)) ? "p" : "n";
    }
    if (!suffix.isEmpty()) {
      suffix = "_" + suffix;
    }

    block.add(
        new IRCallProcess(
            node.getId() + suffix, linearArguments, exponentialArguments, typeArguments));
  }

  @Override
  public void visit(ASTPrintLn node) {
    GeneratedExpression expr = generateExpression(node.getExpr());
    block.add(new IRPrint(expr.getExpr(), node.withNewLine()));
    expr.cleanUp(block, Optional.of(node.getRhs()), true);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTScan node) {
    block.add(new IRScan(record(node.getCh()), intoIRType(node.getType())));
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTUnreachable node) {
    block.add(new IRPanic("Unreachable code reached at line " + node.lineno));
  }

  @Override
  public void visit(ASTCoExpr node) {
    GeneratedExpression expr = generateExpression(node.getExpr());

    block.add(new IRPushExpression(record(node.getCh()), expr.getExpr(), false));
    expr.cleanUp(block, Optional.empty(), true);
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    GeneratedExpression expr = generateExpression(node.getExpr());

    if (optimizeExponentialExpressionToForward && expr.expr instanceof IRExponentialVar) {
      block.add(
          new IRPushExponential(
              record(node.getCh()), ((IRExponentialVar) expr.expr).getExponential()));
    } else {
      block.add(new IRPushExpression(record(node.getCh()), expr.getExpr(), true));
      expr.cleanUp(block, Optional.empty(), true);
    }
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTIf node) {
    Set<String> exponentials = exponentialNamesFreeIn(node);
    GeneratedExpression expr = generateExpression(node.getExpr());

    IRBlock thenBlock = process.addBlock("if_then");
    IRBlock elseBlock = process.addBlock("if_else");

    IRBranch.Case then =
        new IRBranch.Case(thenBlock.getLabel(), countEndPoints(node.getThen()) - 1);
    IRBranch.Case otherwise =
        new IRBranch.Case(elseBlock.getLabel(), countEndPoints(node.getElse()) - 1);
    block.add(new IRBranch(expr.getExpr(), then, otherwise));

    expr.cleanUp(thenBlock, Optional.of(node.getThen()), false);
    expr.cleanUp(elseBlock, Optional.of(node.getElse()), false);

    // Decrement the reference count of any unused exponentials in the branches.
    for (String name : exponentials) {
      decExponentialRefIfUnused(thenBlock, node.getThen(), name);
      decExponentialRefIfUnused(elseBlock, node.getElse(), name);
    }

    visitBlock(thenBlock, node.getThen());
    visitBlock(elseBlock, node.getElse());
  }

  @Override
  public void visit(ASTBang node) {
    // Generate an environment for the process of the exponential closure
    List<ExponentialArgument> exponentialArgs = new ArrayList<>();
    List<TypeArgument> typeArgs = new ArrayList<>();
    Environment env =
        Environment.forExponential(this, environment(), node, exponentialArgs, typeArgs, ep);

    // Generate the process for the exponential closure
    addProcess(
        1,
        exponentialArgs.size(),
        env,
        node.getRhs(),
        entry -> {
          // If the session type is negative, we can't immediately run the closure.
          // Thus, after we enter into the process, we just flip back.
          flipIfNegative(env.record(node.getChi()), node.getType());
        });

    // Call the process we generated above.
    // The reason we use a separate process here at all is to decouple the right
    // hand side of the node from the current environment, so that if it is
    // cloned or cleaned up, it won't clone or destroy the environment we are
    // currently in, and only the environment of the process we are calling now.
    IRBlock finish = process.addBlock("bang_return");
    block.add(new IRNewSession(record(node.getChi()), finish.getLabel()));
    block.add(
        new IRCallProcess(
            env.getName(),
            List.of(new LinearArgument(record(node.getChi()), env.record(node.getChi()))),
            exponentialArgs,
            typeArgs));
    finish.add(new IRNewExponential(exponential(node.getChr() + "$bang"), record(node.getChi())));
    finish.add(new IRPushExponential(record(node.getChr()), exponential(node.getChr() + "$bang")));
    finish.add(new IRDetachExponential(exponential(node.getChr() + "$bang")));
    finish.add(new IRReturn(record(node.getChr())));
  }

  @Override
  public void visit(ASTWhy node) {
    block.add(new IRPopExponential(record(node.getCh()), exponential(node.getCh())));
    block.add(new IRFreeSession(record(node.getCh())));
    decExponentialRefIfUnused(node.getRhs(), node.getCh());
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCall node) {
    boolean decRefCount = !nameFreeIn(node.getRhs(), node.getChr());
    block.add(
        new IRCallExponential(exponential(node.getChr()), record(node.getChi()), decRefCount));

    // We never flip to the called session, even if the session type is negative, as any positive
    // side has already run when the exponential was created.

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTFwdB node) {
    block.add(new IRPushExponential(record(node.getCh1()), exponential(node.getCh2())));
    block.add(new IRReturn(record(node.getCh1())));
  }

  @Override
  public void visit(ASTUnfold node) {
    flip(record(node.getCh()));

    if (node.rec) {
      block.add(new IRPushUnfold(record(node.getCh())));
      flipIfNegative(record(node.getCh()), node.getRhsType());
    } else {
      block.add(new IRPopUnfold(record(node.getCh())));
    }

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTAffine node) {
    // Generate an environment for the process we'll be running.
    List<IRCallProcess.ExponentialArgument> exponentialArgs = new ArrayList<>();
    List<IRCallProcess.LinearArgument> linearArgs = new ArrayList<>();
    List<IRCallProcess.TypeArgument> typeArgs = new ArrayList<>();
    Environment env =
        Environment.forAffine(
            this, environment(), node.getRhs(), exponentialArgs, linearArgs, typeArgs, ep);

    // Generate the process holding the affine right hand side.
    addProcess(
        linearArgs.size(),
        exponentialArgs.size(),
        env,
        node.getRhs(),
        entry -> {
          // If the session type is negative, we should not immediately run the code.
          // Thus, after we enter into the process, we just flip back.
          flipIfNegative(env.record(node.getCh()), node.getContType());
        });

    // Call the process we generated above.
    // The reason we use a separate process here at all is to decouple the right
    // hand side of the node from the current environment, so that if it is
    // cleaned up, it won't destroy the environment we are currently in, and
    // only the environment of the process we are calling now.
    block.add(new IRCallProcess(env.getName(), linearArgs, exponentialArgs, typeArgs));
  }

  @Override
  public void visit(ASTUse node) {
    // We don't generate any code, we can just continue using the channel.
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTDiscard node) {
    // We need to clean up the record as we'll no longer use it.
    block.add(new IRCleanRecord(record(node.getCh())));
    block.add(new IRNextTask());
  }

  @Override
  public void visit(ASTRelease node) {
    block.add(new IRDecRefCell(record(node.getChr())));
    block.add(new IRNextTask());
  }

  @Override
  public void visit(ASTCell node) {
    if (!(node.getTypeRhs() instanceof ASTAffineT && node.getRhs() instanceof ASTAffine)
        && !(node.getTypeRhs() instanceof ASTCellT && node.getRhs() instanceof ASTCell)) {
      throw new RuntimeException(
          "The compiler assumes that the right hand side of a cell is an affine process, or another cell, but got: "
              + node.getRhs().getClass().getSimpleName()
              + " with type "
              + node.getTypeRhs().getClass().getSimpleName());
    }

    IRBlock closure = process.addBlock("cell_closure");

    block.add(new IRNewSession(record(node.getChc()), closure.getLabel()));

    // If an exponential occurs in both sides, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getRhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name)));
      }
    }

    // Flip to the argument if its session type is positive.
    flipIfPositive(record(node.getChc()), node.getTypeRhs());

    block.add(new IRPushCell(record(node.getCh()), record(node.getChc())));
    block.add(new IRReturn(record(node.getCh())));

    if (node.getRhs() instanceof ASTCell) {
      // To avoid duplicating the code, we just wrap the right hand side in an ASTAffine node.
      visitBlock(closure, new ASTAffine(node.getChc(), node.getRhs(), node.getTypeRhs()));
    } else {
      visitBlock(closure, node.getRhs());
    }
  }

  @Override
  public void visit(ASTPut node) {
    if (node.getLhs() instanceof ASTAffine) {
      IRBlock closure = process.addBlock("put_closure");

      block.add(new IRNewSession(record(node.getCho()), closure.getLabel()));

      // If an exponential occurs in both sides, we need to increment its reference count.
      Set<String> exponentials = exponentialNamesFreeIn(node);
      for (String name : exponentials) {
        if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
          block.add(new IRIncRefExponential(exponential(name)));
        }
      }

      // The argument always is affine, and thus, always has a positive type.
      // Thus, we flip to it.
      flip(record(node.getCho()));
      block.add(new IRPutCell(record(node.getChs()), record(node.getCho())));

      // Continue with the right hand side of the put.
      node.getRhs().accept(this);

      visitBlock(closure, node.getLhs());
    } else if (node.getLhs() instanceof ASTFwd) {
      ASTFwd fwd = (ASTFwd) node.getLhs();

      // If the left hand side is a forward, we just forward the value to the channel.
      // This only happens when the left hand side is a cell.
      if (!(fwd.getCh2Type() instanceof ASTUsageT)) {
        throw new RuntimeException(
            "The compiler assumes that the type of forward within a put is usage, but got: "
                + fwd.getCh2Type().getClass().getSimpleName());
      }

      String ch = fwd.getCh1() == node.getCho() ? fwd.getCh2() : fwd.getCh1();
      block.add(new IRPutCell(record(node.getChs()), record(ch)));
      node.getRhs().accept(this);
    } else {
      throw new RuntimeException(
          "The compiler assumes that the left hand side of a put is an affine or forward process, but got: "
              + node.getLhs().getClass().getSimpleName());
    }
  }

  @Override
  public void visit(ASTTake node) {
    if (!(node.getChiType() instanceof ASTCoAffineT) && !(node.getChiType() instanceof ASTUsageT)) {
      throw new RuntimeException(
          "The compiler assumes that its argument has a coaffine or usage type, but got: "
              + node.getChiType().getClass().getSimpleName());
    }

    block.add(new IRTakeCell(record(node.getChr()), record(node.getChi())));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTShare node) {
    IRBlock rhs = process.addBlock("share_rhs");

    // Increment the reference count of the cell.
    block.add(new IRIncRefCell(record(node.getCh())));

    // If an exponential occurs in both branches, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name)));
      }
    }

    if (node.isConcurrent()) {
      block.add(new IRNewThread(rhs.getLabel()));
    } else {
      block.add(new IRNewTask(rhs.getLabel()));
    }
    node.getLhs().accept(this);

    visitBlock(rhs, node.getRhs());
  }

  @Override
  public void visit(ASTSleep node) {
    block.add(new IRSleep(node.getMsecs()));
    node.getRhs().accept(this);
  }

  // ======================================== Utilities =========================================

  private void flip(int record) {
    IRBlock contBlock = process.addBlock("flip");
    block.add(new IRFlip(record, contBlock.getLabel()));
    block = contBlock;
  }

  private void flipIfNegative(int record, ASTType type) {
    if (!isPositive(type)) {
      flip(record);
    }
  }

  private void flipIfPositive(int record, ASTType type) {
    if (isPositive(type)) {
      flip(record);
    }
  }

  private void decExponentialRefIfUnused(IRBlock block, ASTNode node, String name) {
    if (!nameFreeIn(node, name)) {
      block.add(new IRDecRefExponential(exponential(name)));
    }
  }

  private void decExponentialRefIfUnused(ASTNode node, String name) {
    decExponentialRefIfUnused(block, node, name);
  }

  private static Set<String> namesFreeIn(ASTNode node) {
    return node.fn(new HashSet<>());
  }

  private static boolean nameFreeIn(ASTNode node, String name) {
    return namesFreeIn(node).contains(name);
  }

  private Set<String> exponentialNamesFreeIn(ASTNode node) {
    Set<String> names = node.fn(new HashSet<>());
    names.removeIf(name -> !isExponential(name));
    return names;
  }

  private boolean isPositive(ASTType type) {
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
      return dual ^ environment().isPositive(((ASTIdT) type).getid());
    } else {
      return dual ^ type.isPosCatch(ep);
    }
  }

  private IRValueRequisites valueRequisites(int type) {
    if (!optimizeSendValue) {
      return IRValueRequisites.notValue();
    }
    return IRValueRequisites.value(Map.of(), List.of(type));
  }

  private IRValueRequisites valueRequisites(ASTType type, boolean isDual) {
    if (!optimizeSendValue) {
      return IRValueRequisites.notValue();
    }
    return ASTTypeIsValue.check(ep, environment().typeVariables, type, isDual);
  }

  private Environment environment() {
    return environments.peek();
  }

  private int record(String ch) {
    return environment().record(ch);
  }

  private void nextRecord(String ch) {
    environment().nextRecord(ch);
  }

  private void previousRecord(String ch) {
    environment().previousRecord(ch);
  }

  private int exponential(String ch) {
    return environment().exponential(ch);
  }

  private int type(String name) {
    return environment().type(name);
  }

  private boolean isExponential(String ch) {
    return environment().exponentials.containsKey(ch);
  }

  private IRType intoIRType(ASTType type) {
    return environment().intoIRType(ep, type);
  }

  private int countEndPoints(ASTNode node) {
    EndPointCounter counter = new EndPointCounter();
    node.accept(counter);
    return counter.count;
  }

  private class EndPointCounter extends ASTNodeVisitor {
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
    public void visit(ASTShare node) {
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
    public void visit(ASTFwdB node) {}

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
      if (!optimizeSendForward || !(node.getLhs() instanceof ASTFwd)) {
        count += 1;
        node.getLhs().accept(this);
      }
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
    public void visit(ASTScan node) {}

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
      // An extra end point is needed since IRCallProcess is used to call the exponential process.
      count += 1;
    }

    @Override
    public void visit(ASTWhy node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTIf node) {
      node.getThen().accept(this);
      node.getElse().accept(this);
    }

    @Override
    public void visit(ASTSendTy node) {
      count += 1;
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRecvTy node) {
      node.getRhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTAffine node) {}

    @Override
    public void visit(ASTUse node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTDiscard node) {}

    @Override
    public void visit(ASTCell node) {
      count += 1;
    }

    @Override
    public void visit(ASTPut node) {
      if (!(node.getLhs() instanceof ASTFwd)) {
        count += 1;
      }
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTTake node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRelease node) {}

    @Override
    public void visit(ASTSleep node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTUnreachable node) {}
  }

  private class GeneratedExpression {
    private final IRExpression expr;
    private final Set<String> usedRecords;
    private final Set<String> usedExponentials;

    public GeneratedExpression(
        IRExpression expr, Set<String> usedRecords, Set<String> usedExponentials) {
      this.expr = expr;
      this.usedRecords = usedRecords;
      this.usedExponentials = usedExponentials;
    }

    public IRExpression getExpr() {
      return expr;
    }

    public void cleanUp(IRBlock block, Optional<ASTNode> continuation, boolean cleanExponentials) {
      for (String record : usedRecords) {
        block.add(new IRFreeSession(record(record)));
      }
      if (cleanExponentials) {
        for (String exponential : usedExponentials) {
          if (continuation.isEmpty() || !nameFreeIn(continuation.get(), exponential)) {
            block.add(new IRDecRefExponential(exponential(exponential)));
          }
        }
      }
    }
  }

  private GeneratedExpression generateExpression(ASTExpr expr) {
    ExpressionGenerator gen = new ExpressionGenerator();
    expr.accept(gen);
    return new GeneratedExpression(gen.ir, gen.usedRecords, gen.usedExponentials);
  }

  private class ExpressionGenerator extends ASTExprVisitor {
    private IRExpression ir;
    private Set<String> usedRecords = new HashSet<>();
    private Set<String> usedExponentials = new HashSet<>();

    private IRExpression recurse(ASTExpr expr) {
      expr.accept(this);
      return ir;
    }

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
      if (expr.isLinear()) {
        ir = new IRVar(record(expr.getCh()), intoIRType(expr.getType()));
        usedRecords.add(expr.getCh());
      } else {
        ir = new IRExponentialVar(exponential(expr.getCh()), intoIRType(expr.getType()));
        usedExponentials.add(expr.getCh());
      }
    }

    @Override
    public void visit(ASTAdd expr) {
      ir = new IRAdd(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTSub expr) {
      ir = new IRSub(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTMul expr) {
      ir = new IRMul(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTDiv expr) {
      ir = new IRDiv(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTEq expr) {
      ir = new IREq(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTNEq expr) {
      ir = new IRNot(new IREq(recurse(expr.getLhs()), recurse(expr.getRhs())));
    }

    @Override
    public void visit(ASTLt expr) {
      ir = new IRLt(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTGt expr) {
      ir = new IRGt(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTAnd expr) {
      ir = new IRAnd(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTOr expr) {
      ir = new IROr(recurse(expr.getLhs()), recurse(expr.getRhs()));
    }

    @Override
    public void visit(ASTNot expr) {
      ir = new IRNot(recurse(expr.getExpr()));
    }
  }

  public static class Environment {
    private static class RecordLocation {
      public List<Integer> indices = new ArrayList<>();
      public int version = 0;

      public int index() {
        return indices.get(version);
      }
    }

    private String name;
    private final Environment parent;
    private final Map<String, RecordLocation> records = new HashMap<>();
    private final List<IRType> recordTypes = new ArrayList<>();
    private final Map<String, Integer> exponentials = new HashMap<>();
    private final List<IRType> exponentialTypes = new ArrayList<>();
    private final Map<String, Integer> typeVariables = new HashMap<>();
    private final List<Boolean> polarities = new ArrayList<>();
    private int nextChildIndex = 0;
    private final IRGenerator gen;

    // This function takes a process definition, and for each polarity combination of its type
    // arguments,
    // calls the given consumer with a corresponding environment and name suffix.
    public static void forEachProcessPolarity(
        IRGenerator gen,
        ASTProcDef procDef,
        BiConsumer<String, Environment> forEach,
        Env<EnvEntry> ep) {
      Environment env = new Environment(null, procDef.getId(), gen);
      for (int i = 0; i < procDef.getTArgs().size(); ++i) {
        env.insertTypeVariable(procDef.getTArgs().get(i), procDef.getTArgsGen().get(i));
      }
      procDef.getRhs().accept(env.new TypeAssigner());
      for (int i = 0; i < procDef.getArgs().size(); ++i) {
        String arg = procDef.getArgs().get(i);
        ASTType type = procDef.getArgTypes().get(i);
        env.insertLinear(arg, env.intoIRType(ep, type));
      }
      for (int i = 0; i < procDef.getGArgs().size(); ++i) {
        String arg = procDef.getGArgs().get(i);
        ASTType type = procDef.getGArgTypes().get(i);
        env.insertExponential(arg, env.intoIRType(ep, type));
      }
      procDef.getRhs().accept(env.new SessionAssigner(ep));

      // Generate combinations of polarities.
      for (int i = 0; i < (1 << procDef.getTArgs().size()); ++i) {
        String suffix = "";
        for (int j = 0; j < procDef.getTArgs().size(); ++j) {
          String arg = procDef.getTArgs().get(j);
          boolean positive = (i & (1 << j)) != 0;
          env.setPolarity(env.type(arg), positive);
          suffix += (positive ? "p" : "n");
        }
        if (!suffix.isEmpty()) {
          suffix = "_" + suffix;
        }

        env.name = procDef.getId() + suffix;
        forEach.accept(suffix, env);
      }
    }

    private static Environment forExponential(
        IRGenerator gen,
        Environment parent,
        ASTNode node,
        String name,
        String linear,
        List<ExponentialArgument> outInheritedExponentials,
        List<TypeArgument> outInheritedTypes,
        ASTType type,
        Env<EnvEntry> ep) {
      Environment env =
          new Environment(parent, parent.name + "_bang_" + parent.nextChildIndex++, gen);

      // Inherit used exponentials.
      Set<String> freeNames = namesFreeIn(node);
      for (Map.Entry<String, Integer> entry : parent.exponentials.entrySet()) {
        if (freeNames.contains(entry.getKey()) && entry.getKey() != linear) {
          int index =
              env.insertExponential(entry.getKey(), parent.exponentialTypes.get(entry.getValue()));
          outInheritedExponentials.add(new ExponentialArgument(entry.getValue(), index));
        }
      }

      // Inherit all types in the parent environemnt.
      for (int i = 0; i < parent.typeVariableCount(); ++i) {
        // Find name of the type variable.
        String typeName = null;
        String typeGenName = null;
        for (Map.Entry<String, Integer> entry : parent.typeVariables.entrySet()) {
          if (entry.getValue() == i) {
            if (typeName == null) {
              typeName = entry.getKey();
            } else {
              typeGenName = entry.getKey();
              break; // We found both names, no need to continue.
            }
          }
        }

        if (typeName == null) {
          throw new IllegalStateException("Type variable not found in parent environment");
        }
        if (typeGenName == null) {
          typeGenName = typeName;
        }

        // Insert the type variable into the new environment.
        int index = env.insertTypeVariable(typeName, typeGenName);
        env.setPolarity(index, parent.isPositive(typeGenName));
        outInheritedTypes.add(
            new TypeArgument(
                new IRVarT(i, Optional.empty()),
                gen.valueRequisites(i),
                parent.isPositive(typeGenName),
                index));
      }

      if (linear != null) {
        env.insertLinear(linear, env.intoIRType(ep, type));
      }

      node.accept(env.new TypeAssigner());
      node.accept(env.new SessionAssigner(ep));
      return env;
    }

    public static Environment forExponential(
        IRGenerator gen,
        Environment parent,
        ASTBang node,
        List<ExponentialArgument> outInheritedExponentials,
        List<TypeArgument> outInheritedTypes,
        Env<EnvEntry> ep) {
      return forExponential(
          gen,
          parent,
          node.getRhs(),
          node.getChr(),
          node.getChi(),
          outInheritedExponentials,
          outInheritedTypes,
          node.getType(),
          ep);
    }

    public static Environment forAffine(
        IRGenerator gen,
        Environment parent,
        ASTNode node,
        List<IRCallProcess.ExponentialArgument> outExponentialArgs,
        List<IRCallProcess.LinearArgument> outLinearArgs,
        List<IRCallProcess.TypeArgument> outTypeArgs,
        Env<EnvEntry> ep) {
      Environment env =
          new Environment(parent, parent.name + "_affine_" + parent.nextChildIndex++, gen);

      // Pass all exponentials ocurring in the node.
      Set<String> freeNames = namesFreeIn(node);
      for (Map.Entry<String, Integer> entry : parent.exponentials.entrySet()) {
        if (freeNames.contains(entry.getKey())) {
          int index =
              env.insertExponential(entry.getKey(), parent.exponentialTypes.get(entry.getValue()));
          outExponentialArgs.add(new IRCallProcess.ExponentialArgument(entry.getValue(), index));
        }
      }

      // Pass all linear arguments ocurring in the node.
      for (Map.Entry<String, RecordLocation> entry : parent.records.entrySet()) {
        if (freeNames.contains(entry.getKey())
            && !parent.exponentials.containsKey(entry.getKey())) {
          int index =
              env.insertLinear(entry.getKey(), parent.recordTypes().get(entry.getValue().index()));
          outLinearArgs.add(new IRCallProcess.LinearArgument(entry.getValue().index(), index));
        }
      }

      // Pass all types ocurring in the node.
      for (int i = 0; i < parent.typeVariableCount(); ++i) {
        // Find name of the type variable.
        String typeName = null;
        String typeGenName = null;
        for (Map.Entry<String, Integer> entry : parent.typeVariables.entrySet()) {
          if (entry.getValue() == i) {
            if (typeName == null) {
              typeName = entry.getKey();
            } else {
              typeGenName = entry.getKey();
              break; // We found both names, no need to continue.
            }
          }
        }

        if (typeName == null) {
          throw new IllegalStateException("Type variable not found in parent environment");
        }
        if (typeGenName == null) {
          typeGenName = typeName;
        }

        // Insert the type variable into the new environment.
        int index = env.insertTypeVariable(typeName, typeGenName);
        env.setPolarity(index, parent.isPositive(typeGenName));
        outTypeArgs.add(
            new IRCallProcess.TypeArgument(
                new IRVarT(i, Optional.empty()),
                gen.valueRequisites(i),
                parent.isPositive(typeGenName),
                index));
      }

      node.accept(env.new TypeAssigner());
      node.accept(env.new SessionAssigner(ep));
      return env;
    }

    private Environment(Environment parent, String name, IRGenerator gen) {
      this.parent = parent;
      this.name = name;
      this.gen = gen;
    }

    public List<IRType> recordTypes() {
      return recordTypes;
    }

    public int record(String session) {
      return records.get(session).index();
    }

    public void nextRecord(String session) {
      records.get(session).version += 1;
    }

    private void previousRecord(String session) {
      records.get(session).version -= 1;
    }

    public List<IRType> exponentialTypes() {
      return exponentialTypes;
    }

    public int exponential(String session) {
      return exponentials.get(session);
    }

    public int type(String name) {
      return typeVariables.get(name);
    }

    public int typeVariableCount() {
      return polarities.size();
    }

    public List<Boolean> typeVariablePolarities() {
      return new ArrayList<>(polarities);
    }

    public boolean isPositive(String typeVar) {
      if (!typeVariables.containsKey(typeVar)) {
        if (parent == null) {
          throw new IllegalStateException("Type variable " + typeVar + " not found in environment");
        }
        return parent.isPositive(typeVar);
      }
      return polarities.get(typeVariables.get(typeVar));
    }

    public void setPolarity(int type, boolean isPositive) {
      polarities.set(type, isPositive);
    }

    public IRType intoIRType(Env<EnvEntry> ep, ASTType type) {
      return ASTIntoIRType.convert(gen, ep, type, typeVariables);
    }

    public String getName() {
      return name;
    }

    private int insertLinear(String session, IRType type) {
      if (!records.containsKey(session)) {
        records.put(session, new RecordLocation());
      }
      int index = recordTypes.size();
      records.get(session).indices.add(index);
      recordTypes.add(type);
      return index;
    }

    private int insertExponential(String session, IRType type) {
      int index = exponentials.size();
      exponentials.put(session, index);
      exponentialTypes.add(type);
      return index;
    }

    private int insertTypeVariable(String name, String genName) {
      int index = polarities.size();
      typeVariables.put(name, index);
      typeVariables.put(genName, index);
      polarities.add(true);
      return index;
    }

    // A visitor which traverses the AST and assigns an index to each type variable received in it.
    // Replication right-hand-sides are ignored.
    private class TypeAssigner extends ASTNodeVisitor {
      @Override
      public void visit(ASTNode node) {
        throw new UnsupportedOperationException(
            "Nodes of type "
                + node.getClass().getName()
                + " are not yet supported by Environment.TypeAssigner");
      }

      @Override
      public void visit(ASTBang node) {}

      @Override
      public void visit(ASTEmpty node) {}

      @Override
      public void visit(ASTFwd node) {}

      @Override
      public void visit(ASTFwdB node) {}

      @Override
      public void visit(ASTId node) {}

      @Override
      public void visit(ASTMix node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTCall node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTIf node) {
        node.getThen().accept(this);
        node.getElse().accept(this);
      }

      @Override
      public void visit(ASTPromoCoExpr node) {}

      @Override
      public void visit(ASTCoExpr node) {}

      @Override
      public void visit(ASTWhy node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTUnfold node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSend node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTRecv node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSelect node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTCase node) {
        for (ASTNode branch : node.getCases().values()) {
          branch.accept(this);
        }
      }

      @Override
      public void visit(ASTPrintLn node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTScan node) {}

      @Override
      public void visit(ASTCut node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTClose node) {}

      @Override
      public void visit(ASTCoClose node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTRecvTy node) {
        insertTypeVariable(node.getTyidPar(), node.getTyidGen());
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSendTy node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTExpr node) {}

      @Override
      public void visit(ASTAffine node) {}

      @Override
      public void visit(ASTUse node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTDiscard node) {}

      @Override
      public void visit(ASTRelease node) {}

      @Override
      public void visit(ASTCell node) {}

      @Override
      public void visit(ASTPut node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTTake node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTShare node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSleep node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTUnreachable node) {}
    }

    // A visitor which simply traverses the AST and assigns an index to each session created in it.
    // Replication right-hand-sides are ignored.
    private class SessionAssigner extends ASTNodeVisitor {
      private Env<EnvEntry> ep;

      public SessionAssigner(Env<EnvEntry> ep) {
        this.ep = ep;
      }

      @Override
      public void visit(ASTNode node) {
        throw new UnsupportedOperationException(
            "Nodes of type "
                + node.getClass().getName()
                + " are not yet supported by Environment.SessionAssigner");
      }

      @Override
      public void visit(ASTBang node) {
        insertExponential(node.getChr() + "$bang", intoIRType(ep, node.getType()));
        insertLinear(node.getChi(), intoIRType(ep, node.getType()));
      }

      @Override
      public void visit(ASTEmpty node) {}

      @Override
      public void visit(ASTFwd node) {}

      @Override
      public void visit(ASTFwdB node) {}

      @Override
      public void visit(ASTId node) {}

      @Override
      public void visit(ASTMix node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTCall node) {
        insertLinear(node.getChi(), intoIRType(ep, node.getType()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTIf node) {
        node.getThen().accept(this);
        node.getElse().accept(this);
      }

      @Override
      public void visit(ASTPromoCoExpr node) {}

      @Override
      public void visit(ASTCoExpr node) {}

      @Override
      public void visit(ASTWhy node) {
        insertExponential(node.getCh(), intoIRType(ep, node.getType()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTUnfold node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSend node) {
        if (!gen.optimizeSendForward || !(node.getLhs() instanceof ASTFwd)) {
          insertLinear(node.getCho(), intoIRType(ep, node.getLhsType()));
          node.getLhs().accept(this);
        }
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTRecv node) {
        insertLinear(node.getChi(), intoIRType(ep, node.getChiType()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSelect node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTCase node) {
        for (ASTNode branch : node.getCases().values()) {
          branch.accept(this);
        }
      }

      @Override
      public void visit(ASTPrintLn node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTScan node) {}

      @Override
      public void visit(ASTCut node) {
        insertLinear(node.getCh(), intoIRType(ep, node.getChType()));
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTClose node) {}

      @Override
      public void visit(ASTCoClose node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTRecvTy node) {
        EnvEntry entry = new TypeEntry(new ASTIdT(node.getTyidGen()));
        ep = ep.assoc(node.getTyid(), entry);
        ep = ep.assoc(node.getTyidGen(), entry);
        ep = ep.assoc(node.getTyidPar(), entry);
        insertLinear(node.getChs(), intoIRType(ep, node.getTypeRhs()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSendTy node) {
        insertLinear(node.getChs(), intoIRType(ep, node.getTypeRhs()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTExpr node) {}

      @Override
      public void visit(ASTAffine node) {}

      @Override
      public void visit(ASTUse node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTDiscard node) {}

      @Override
      public void visit(ASTRelease node) {}

      @Override
      public void visit(ASTCell node) {
        insertLinear(node.getChc(), intoIRType(ep, node.getTypeRhs()));
      }

      @Override
      public void visit(ASTPut node) {
        insertLinear(node.getCho(), intoIRType(ep, node.getLhsType()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTTake node) {
        insertLinear(node.getChi(), intoIRType(ep, node.getChiType()));
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTShare node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSleep node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTUnreachable node) {}
    }
  }
}
