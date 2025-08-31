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
import pt.inescid.cllsj.compiler.ir.expressions.*;
import pt.inescid.cllsj.compiler.ir.instructions_old.*;
import pt.inescid.cllsj.compiler.ir.instructions_old.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.instructions_old.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions_old.IRCallProcess.TypeArgument;
import pt.inescid.cllsj.compiler.ir.type.IRCloseT;
import pt.inescid.cllsj.compiler.ir.type.IRExponentialT;
import pt.inescid.cllsj.compiler.ir.type.IRResetT;
import pt.inescid.cllsj.compiler.ir.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRTypeT;
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
            addProcess(
                procDef.getArgs().size(),
                procDef.getGArgs().size(),
                env,
                procDef.getRhs(),
                true,
                procDef.isRecursive());
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
      boolean inlineable,
      boolean recursive) {
    IRProcess oldProcess = process;
    process =
        new IRProcess(
            linearArgumentCount,
            exponentialArgumentCount,
            env.recordCount(),
            env.exponentialCount(),
            env.typeVariablePolarities(),
            countEndPoints(node),
            inlineable,
            recursive);
    program.addProcess(env.getName(), process);
    environments.push(env);
    visitBlock(process.getEntry(), node);
    environments.pop();
    process = oldProcess;
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
        block.add(new IRIncRefExponential(exponential(name), exponentialType(name)));
      }
    }

    IRBlock negBlock = process.addBlock(negLabel);
    block.add(
        new IRNewSession(record(node.getCh()), negBlock.getLabel(), intoIRType(node.getChType())));
    reset(record(node.getCh()), node.getChType());
    pos.accept(this);
    visitBlock(
        negBlock,
        () -> {
          reset(record(node.getCh()), node.getChType());
          neg.accept(this);
        });
  }

  @Override
  public void visit(ASTMix node) {
    IRBlock rhs = process.addBlock("mix_rhs");

    // If an exponential occurs in both branches, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name), exponentialType(name)));
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
      block.add(
          new IRForward(
              record(node.getCh1()), record(node.getCh2()), intoIRType(node.getCh2Type())));
    } else {
      block.add(
          new IRForward(
              record(node.getCh2()), record(node.getCh1()), intoIRType(node.getCh2Type())));
    }
  }

  @Override
  public void visit(ASTEmpty node) {
    block.add(new IRNextTask());
  }

  @Override
  public void visit(ASTClose node) {
    block.add(new IRPushClose(record(node.getCh()), new IRCloseT()));
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTCoClose node) {
    block.add(new IRPopClose(record(node.getCh()), new IRCloseT()));
    block.add(new IRFreeSession(record(node.getCh())));
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSend node) {
    // If an exponential occurs in both sides, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name), exponentialType(name)));
      }
    }

    int pushedRecord;
    if (optimizeSendForward && node.getLhs() instanceof ASTFwd) {
      ASTFwd fwd = (ASTFwd) node.getLhs();
      String ch = fwd.getCh1().equals(node.getCho()) ? fwd.getCh2() : fwd.getCh1();

      pushedRecord = record(ch);
    } else {
      pushedRecord = record(node.getCho());

      IRBlock closure = process.addBlock("send_closure");
      block.add(new IRNewSession(pushedRecord, closure.getLabel(), intoIRType(node.getLhsType())));

      // Flip to the argument if its session type is positive.
      // This execution order is different from the one in the SAM specification
      flipIfPositive(pushedRecord, node.getLhsType());

      visitBlock(
          closure,
          () -> {
            reset(pushedRecord, node.getLhsType());
            node.getLhs().accept(this);
          });

      reset(pushedRecord, node.getLhsType());
    }

    IRType type =
        new IRSessionT(intoIRType(node.getLhsType(), false), intoIRType(node.getRhsType(), false));
    block.add(
        new IRPushSession(
            record(node.getChs()), type, pushedRecord, intoIRType(node.getLhsType())));

    // Flip if the remainder of the session type is negative.
    flipAndResetIfNegative(record(node.getChs()), node.getRhsType());

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRecv node) {
    IRType type =
        new IRSessionT(intoIRType(node.getChiType(), true), intoIRType(node.getRhsType(), true));
    block.add(
        new IRPopSession(
            record(node.getChr()), type, record(node.getChi()), intoIRType(node.getChiType())));
    resetIfPositive(record(node.getChr()), node.getRhsType());
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
    block.add(new IRNewSession(nextRecord, closure.getLabel(), intoIRType(node.getTypeRhs())));
    flipIfPositive(nextRecord, node.getTypeRhs());
    reset(nextRecord, node.getTypeRhs());
    block.add(
        new IRPushType(
            previousRecord,
            new IRTypeT(intoIRType(node.getTypeRhs())),
            nextRecord,
            intoIRType(node.getType()),
            isPositive(node.getType())));
    block.add(new IRReturn(previousRecord));

    // Generate code for the node's continuation.
    visitBlock(
        closure,
        () -> {
          reset(nextRecord, node.getTypeRhs());
          node.getRhs().accept(this);
          ;
        });
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

    block.add(
        new IRPopType(
            previousRecord,
            new IRTypeT(intoIRType(node.getTypeRhs())),
            nextRecord,
            type(node.getTyidGen()),
            new IRPopType.Case(positiveBlock.getLabel(), 0),
            new IRPopType.Case(negativeBlock.getLabel(), 0)));
    positiveBlock.add(new IRFreeSession(previousRecord));
    negativeBlock.add(new IRFreeSession(previousRecord));

    // Generate code for each possible polarity.
    environment().setPolarity(type(node.getTyidGen()), true);
    visitBlock(
        positiveBlock,
        () -> {
          resetIfPositive(nextRecord, node.getTypeRhs());
          node.getRhs().accept(this);
        });
    environment().setPolarity(type(node.getTyidGen()), false);
    visitBlock(
        negativeBlock,
        () -> {
          resetIfPositive(nextRecord, node.getTypeRhs());
          node.getRhs().accept(this);
        });

    previousRecord(node.getChs());
  }

  @Override
  public void visit(ASTSelect node) {
    block.add(
        new IRPushTag(record(node.getCh()), intoIRType(node.getCaseType()), node.getLabelIndex()));

    // Flip if the remainder of the session type is negative.
    flipAndResetIfNegative(record(node.getCh()), node.getRhsType());

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCase node) {
    Set<String> exponentials = exponentialNamesFreeIn(node);
    Map<Integer, IRPopTag.Case> cases = new HashMap<>();
    block.add(new IRPopTag(record(node.getCh()), intoIRType(node.getOfferType()), cases));

    for (int i = 0; i < node.getCaseCount(); ++i) {
      String caseLabel = node.getCaseLabelFromIndex(i);
      ASTNode caseNode = node.getCase(caseLabel);

      IRBlock caseBlock = process.addBlock("case_" + caseLabel.substring(1));
      cases.put(i, new IRPopTag.Case(caseBlock.getLabel(), countEndPoints(caseNode)));

      // Decrement the reference count of any unused exponentials in this case.
      for (String name : exponentials) {
        decExponentialRefIfUnused(caseBlock, caseNode, name);
      }

      visitBlock(
          caseBlock,
          () -> {
            resetIfPositive(record(node.getCh()), node.getCaseType(caseLabel));
            caseNode.accept(this);
          });
    }
  }

  @Override
  public void visit(ASTId node) {
    List<LinearArgument> linearArguments = new ArrayList<>();
    List<ExponentialArgument> exponentialArguments = new ArrayList<>();
    List<TypeArgument> typeArguments = new ArrayList<>();

    for (int i = 0; i < node.getPars().size(); ++i) {
      linearArguments.add(
          new LinearArgument(
              record(node.getPars().get(i)), i, intoIRType(node.getParTypes().get(i))));
    }

    for (int i = 0; i < node.getGPars().size(); ++i) {
      exponentialArguments.add(
          new ExponentialArgument(
              exponential(node.getGPars().get(i)),
              i,
              intoIRType(node.getGParTypes().get(i), false)));
    }

    for (int i = 0; i < node.getTPars().size(); ++i) {
      typeArguments.add(
          new TypeArgument(
              intoIRType(node.getTPars().get(i)), isPositive(node.getTPars().get(i)), i));
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
    block.add(new IRPushScan(record(node.getCh()), intoIRType(node.getType())));
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTUnreachable node) {
    block.add(new IRPanic("Unreachable code reached at line " + node.lineno));
  }

  @Override
  public void visit(ASTCoExpr node) {
    GeneratedExpression expr = generateExpression(node.getExpr());

    block.add(
        new IRPushExpression(
            record(node.getCh()), expr.getExpr().getType(), expr.getExpr(), false));
    expr.cleanUp(block, Optional.empty(), true);
    block.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    GeneratedExpression expr = generateExpression(node.getExpr());

    if (optimizeExponentialExpressionToForward && expr.expr instanceof IRExponentialVar) {
      block.add(
          new IRPushExponential(
              record(node.getCh()),
              new IRExponentialT(expr.getExpr().getType()),
              ((IRExponentialVar) expr.expr).getExponential()));
    } else {
      block.add(
          new IRPushExpression(
              record(node.getCh()), expr.getExpr().getType(), expr.getExpr(), true));
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

    IRBranch.Case then = new IRBranch.Case(thenBlock.getLabel(), countEndPoints(node.getThen()));
    IRBranch.Case otherwise =
        new IRBranch.Case(elseBlock.getLabel(), countEndPoints(node.getElse()));
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
    addProcess(1, exponentialArgs.size(), env, node.getRhs(), false, false);

    // Use the process we generated above to create the exponential.
    block.add(
        new IRNewExponentialProcess(
            exponential(node.getChr() + "$bang"),
            exponentialType(node.getChr() + "$bang"),
            env.getName(),
            exponentialArgs,
            typeArgs));
    block.add(
        new IRPushExponential(
            record(node.getChr()),
            intoIRType(new ASTBangT(node.getType())),
            exponential(node.getChr() + "$bang")));
    block.add(new IRReturn(record(node.getChr())));
  }

  @Override
  public void visit(ASTWhy node) {
    block.add(
        new IRPopExponential(
            record(node.getCh()),
            new IRExponentialT(exponentialType(node.getCh())),
            exponential(node.getCh())));
    block.add(new IRFreeSession(record(node.getCh())));
    decExponentialRefIfUnused(node.getRhs(), node.getCh());
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCall node) {
    block.add(
        new IRCallExponential(
            exponential(node.getChr()), exponentialType(node.getChr()), record(node.getChi())));

    if (!nameFreeIn(node.getRhs(), node.getChr())) {
      block.add(
          new IRDecRefExponential(exponential(node.getChr()), exponentialType(node.getChr())));
    }

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTFwdB node) {
    block.add(
        new IRPushExponential(
            record(node.getCh1()), intoIRType(node.getType()), exponential(node.getCh2())));
    block.add(new IRReturn(record(node.getCh1())));
  }

  @Override
  public void visit(ASTUnfold node) {
    flipAndReset(record(node.getCh()), node.getRhsType());

    if (node.rec) {
      flipIfNegative(record(node.getCh()), node.getRhsType());
    }

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTAffine node) {
    // Generate a pop tag instruction
    IRBlock discardBlock = process.addBlock("affine_discard");
    IRBlock useBlock = process.addBlock("affine_use");
    Map<Integer, IRPopTag.Case> cases = new HashMap<>();
    block.add(
        new IRPopTag(record(node.getCh()), intoIRType(new ASTAffineT(node.getContType())), cases));

    // Generate the use:
    // - we just execute the continuation of the affine, nothing special
    cases.put(1, new IRPopTag.Case(useBlock.getLabel(), countEndPoints(node.getRhs())));
    visitBlock(
        useBlock,
        () -> {
          resetIfPositive(record(node.getCh()), node.getContType());
          node.getRhs().accept(this);
        });

    // Generate the discard:
    // - we must discard any inherited affines
    // - decrement the reference count of unused cells and exponentials
    cases.put(0, new IRPopTag.Case(discardBlock.getLabel(), 1));

    for (String name : exponentialNamesFreeIn(node)) {
      discardBlock.add(new IRDecRefExponential(exponential(name), exponentialType(name)));
    }

    for (String name : node.getUsageSet().keySet()) {
      discardBlock.add(new IRDecRefCell(record(name)));
    }

    for (String name : node.getCoaffineSet().keySet()) {
      ASTType type = node.getCoaffineSet().get(name);

      IRBlock contBlock = process.addBlock("affine_discard_flip");

      discardBlock.add(new IRPushTag(record(name), intoIRType(type), 0));
      discardBlock.add(new IRFlip(record(name), new IRCloseT(), contBlock.getLabel()));
      discardBlock = contBlock;
      discardBlock.add(new IRPopClose(record(name), new IRCloseT()));
      discardBlock.add(new IRFreeSession(record(name)));
    }

    discardBlock.add(new IRPushClose(record(node.getCh()), new IRCloseT()));
    discardBlock.add(new IRReturn(record(node.getCh())));
  }

  @Override
  public void visit(ASTUse node) {
    // Tag 1 represents use for affine records
    block.add(
        new IRPushTag(record(node.getCh()), intoIRType(new ASTCoAffineT(node.getContType())), 1));

    // Flip if the remainder of the session type is negative.
    flipAndResetIfNegative(record(node.getCh()), node.getContType());

    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTDiscard node) {
    IRBlock contBlock = process.addBlock("discard_flip");

    // Tag 0 represents discard for affine records
    block.add(new IRPushTag(record(node.getCh()), intoIRType(node.getCoAffineT()), 0));
    block.add(new IRFlip(record(node.getCh()), new IRCloseT(), contBlock.getLabel()));
    contBlock.add(new IRPopClose(record(node.getCh()), new IRCloseT()));
    contBlock.add(new IRFreeSession(record(node.getCh())));
    contBlock.add(new IRNextTask());
  }

  @Override
  public void visit(ASTRelease node) {
    block.add(new IRDecRefCell(record(node.getChr())));
    block.add(new IRNextTask());
  }

  @Override
  public void visit(ASTCell node) {
    throw new UnsupportedOperationException("Cells are not supported yet");
  }

  @Override
  public void visit(ASTPut node) {
    throw new UnsupportedOperationException("Cells are not supported yet");
  }

  @Override
  public void visit(ASTTake node) {
    throw new UnsupportedOperationException("Cells are not supported yet");
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
        block.add(new IRIncRefExponential(exponential(name), exponentialType(name)));
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

  private void reset(int record, ASTType type) {
    block.add(new IRResetSession(record, intoIRType(type)));
  }

  private void resetIfPositive(int record, ASTType type) {
    if (isPositive(type)) {
      block.add(new IRResetSession(record, intoIRType(type)));
    }
  }

  private void flip(int record, ASTType contType) {
    IRBlock contBlock = process.addBlock("flip");
    block.add(new IRFlip(record, intoIRType(contType), contBlock.getLabel()));
    block = contBlock;
  }

  private void flipAndReset(int record, ASTType contType) {
    flip(record, contType);
    reset(record, contType);
  }

  private void flipIfPositive(int record, ASTType type) {
    if (isPositive(type)) {
      flip(record, type);
    }
  }

  private void flipIfNegative(int record, ASTType type) {
    if (!isPositive(type)) {
      flip(record, type);
    }
  }

  private void flipAndResetIfNegative(int record, ASTType type) {
    if (!isPositive(type)) {
      flipAndReset(record, type);
    }
  }

  private void decExponentialRefIfUnused(IRBlock block, ASTNode node, String name) {
    if (!nameFreeIn(node, name)) {
      block.add(new IRDecRefExponential(exponential(name), exponentialType(name)));
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
    return environment().isPositive(ep, type);
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

  private IRType exponentialType(String ch) {
    return intoIRType(environment().exponentialType(ch), false);
  }

  private int type(String name) {
    return environment().type(name);
  }

  private boolean isExponential(String ch) {
    return environment().exponentials.containsKey(ch);
  }

  private IRType intoIRType(ASTType type, boolean resetPolarity) {
    return environment().intoIRType(ep, type, resetPolarity);
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
    private int count = 0;

    @Override
    public void visit(ASTNode node) {
      throw new UnsupportedOperationException(
          "Unsupported AST node: " + node.getClass().getSimpleName());
    }

    @Override
    public void visit(ASTCut node) {
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTMix node) {
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTShare node) {
      node.getLhs().accept(this);
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTEmpty node) {
      count += 1;
    }

    @Override
    public void visit(ASTClose node) {
      count += 1;
    }

    @Override
    public void visit(ASTId node) {
      count += 1;
    }

    @Override
    public void visit(ASTFwd node) {
      count += 1;
    }

    @Override
    public void visit(ASTFwdB node) {
      count += 1;
    }

    @Override
    public void visit(ASTCoExpr node) {
      count += 1;
    }

    @Override
    public void visit(ASTPromoCoExpr node) {
      count += 1;
    }

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
      int oldCount = count;
      int maxCount = 0;
      for (int i = 0; i < node.getCaseCount(); ++i) {
        count = 0;
        node.getCase(node.getCaseLabelFromIndex(i)).accept(this);
        maxCount = Math.max(maxCount, count);
      }
      count = oldCount + maxCount;
    }

    @Override
    public void visit(ASTSend node) {
      if (!optimizeSendForward || !(node.getLhs() instanceof ASTFwd)) {
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
    public void visit(ASTScan node) {
      count += 1;
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
      count += 1;
    }

    @Override
    public void visit(ASTWhy node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTIf node) {
      int oldCount = count;
      count = 0;
      node.getThen().accept(this);
      int maxCount = count;
      count = 0;
      node.getElse().accept(this);
      maxCount = Math.max(maxCount, count);
      count = oldCount + maxCount;
    }

    @Override
    public void visit(ASTSendTy node) {
      count += 1;
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRecvTy node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTAffine node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTUse node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTDiscard node) {
      count += 1;
    }

    @Override
    public void visit(ASTCell node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTPut node) {
      if (!(node.getLhs() instanceof ASTFwd)) {
        node.getLhs().accept(this);
      }
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTTake node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRelease node) {
      count += 1;
    }

    @Override
    public void visit(ASTSleep node) {
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTUnreachable node) {
      count += 1;
    }
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
            block.add(
                new IRDecRefExponential(exponential(exponential), exponentialType(exponential)));
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
    private int recordCount = 0;
    private final Map<String, Integer> exponentials = new HashMap<>();
    private final List<ASTType> exponentialTypes = new ArrayList<>();
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
        env.insertLinear(arg);
      }
      for (int i = 0; i < procDef.getGArgs().size(); ++i) {
        String arg = procDef.getGArgs().get(i);
        env.insertExponential(arg, procDef.getGArgTypes().get(i));
      }
      procDef.getRhs().accept(env.new SessionAssigner());

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
          ASTType argType = parent.exponentialTypes.get(entry.getValue());
          int index = env.insertExponential(entry.getKey(), argType);
          outInheritedExponentials.add(
              new ExponentialArgument(
                  entry.getValue(), index, parent.intoIRType(ep, argType, false)));
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
                new IRVarT(i, Optional.empty()), parent.isPositive(typeGenName), index));
      }

      if (linear != null) {
        env.insertLinear(linear);
      }

      node.accept(env.new TypeAssigner());
      node.accept(env.new SessionAssigner());
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

    private Environment(Environment parent, String name, IRGenerator gen) {
      this.parent = parent;
      this.name = name;
      this.gen = gen;
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

    public int recordCount() {
      return recordCount;
    }

    public int exponentialCount() {
      return exponentials.size();
    }

    public int exponential(String session) {
      return exponentials.get(session);
    }

    public ASTType exponentialType(String session) {
      return exponentialTypes.get(exponentials.get(session));
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

    public boolean isPositive(Env<EnvEntry> ep, ASTType type) {
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
        return dual ^ isPositive(((ASTIdT) type).getid());
      } else {
        return dual ^ type.getPolarityForCompilerCatch(ep).get();
      }
    }

    public IRType intoIRType(Env<EnvEntry> ep, ASTType type) {
      return ASTIntoIRType.convert(gen, ep, type, typeVariables);
    }

    public IRType intoIRType(Env<EnvEntry> ep, ASTType type, boolean resetPolarity) {
      IRType converted = intoIRType(ep, type);
      if (isPositive(ep, type) == resetPolarity) {
        return new IRResetT(converted);
      } else {
        return converted;
      }
    }

    public String getName() {
      return name;
    }

    private int insertLinear(String session) {
      if (!records.containsKey(session)) {
        records.put(session, new RecordLocation());
      }
      int index = recordCount++;
      records.get(session).indices.add(index);
      return index;
    }

    private int insertExponential(String session, ASTType type) {
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
      public void visit(ASTAffine node) {
        node.getRhs().accept(this);
      }

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
      @Override
      public void visit(ASTNode node) {
        throw new UnsupportedOperationException(
            "Nodes of type "
                + node.getClass().getName()
                + " are not yet supported by Environment.SessionAssigner");
      }

      @Override
      public void visit(ASTBang node) {
        insertExponential(node.getChr() + "$bang", node.getType());
        insertLinear(node.getChi());
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
        insertLinear(node.getChi());
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
        try {
          insertExponential(node.getCh(), node.getType().dual(new Env<>()));
        } catch (Exception e) {
          throw new RuntimeException(e);
        }
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTUnfold node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSend node) {
        if (!gen.optimizeSendForward || !(node.getLhs() instanceof ASTFwd)) {
          insertLinear(node.getCho());
          node.getLhs().accept(this);
        }
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTRecv node) {
        insertLinear(node.getChi());
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
        insertLinear(node.getCh());
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
        insertLinear(node.getChs());
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSendTy node) {
        insertLinear(node.getChs());
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTExpr node) {}

      @Override
      public void visit(ASTAffine node) {
        node.getRhs().accept(this);
      }

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
        insertLinear(node.getChc());
      }

      @Override
      public void visit(ASTPut node) {
        insertLinear(node.getCho());
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTTake node) {
        insertLinear(node.getChi());
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
