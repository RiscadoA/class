package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.ExponentialArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRCallProcess.LinearArgument;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushExponential.InheritedExponential;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRGenerator extends ASTNodeVisitor {
  private IRProgram program = new IRProgram();
  private IRProcess process;
  private IRBlock block;
  private Stack<Environment> environments = new Stack<>();
  private Env<EnvEntry> ep;

  public static IRProgram generate(Env<EnvEntry> ep, ASTProgram ast) {
    final IRGenerator gen = new IRGenerator();

    for (ASTProcDef procDef : ast.getProcDefs()) {
      gen.ep = ep;
      for (String arg : procDef.getTArgs()) {
        gen.ep = gen.ep.assoc(arg, new TypeEntry(new ASTIdT(arg)));
      }

      Environment.forEachProcessPolarity(
          procDef,
          (suffix, env) -> {
            gen.process =
                new IRProcess(
                    procDef.hasArguments(),
                    env.recordCount(),
                    env.exponentialCount(),
                    countEndPoints(procDef.getRhs()));
            gen.program.addProcess(procDef.getId() + suffix, gen.process);
            gen.environments.push(env);
            gen.visitBlock(gen.process.getEntry(), procDef.getRhs());
            gen.environments.pop();
          });
    }

    return gen.program;
  }

  // ==================================== AST node visitors =====================================

  private void visitBlock(IRBlock block, ASTNode node) {
    this.block = block;
    node.accept(this);
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

    IRType type = ASTIntoIRType.convert(ep, node.getChType());
    IRBlock negBlock = process.addBlock(negLabel);
    block.add(new IRNewSession(record(node.getCh()), type, negBlock.getLabel()));
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

    block.add(new IRNewTask(rhs.getLabel()));
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
    IRBlock closure = process.addBlock("send_closure");
    IRType type = ASTIntoIRType.convert(ep, node.getLhsType());

    block.add(new IRNewSession(record(node.getCho()), type, closure.getLabel()));
    block.add(new IRPushSession(record(node.getChs()), record(node.getCho())));

    // If an exponential occurs in both sides, we need to increment its reference count.
    Set<String> exponentials = exponentialNamesFreeIn(node);
    for (String name : exponentials) {
      if (nameFreeIn(node.getLhs(), name) && nameFreeIn(node.getRhs(), name)) {
        block.add(new IRIncRefExponential(exponential(name)));
      }
    }

    // Flip if the remainder of the session type is negative.
    if (!isPositive(node.getRhsType())) {
      block.add(new IRFlip(record(node.getChs())));
    }
    node.getRhs().accept(this);

    visitBlock(closure, node.getLhs());
  }

  @Override
  public void visit(ASTRecv node) {
    block.add(new IRPopSession(record(node.getChr()), record(node.getChi())));

    // Flip to the received session if it is negative.
    if (!isPositive(node.getChiType())) {
      block.add(new IRFlip(record(node.getChi())));
    }
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTSendTy node) {
    block.add(new IRPushType(record(node.getChs()), isPositive(node.getType())));

    // Flip if the remainder of the session type is negative.
    if (!isPositive(node.getTypeRhs())) {
      block.add(new IRFlip(record(node.getChs())));
    }
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTRecvTy node) {
    IRBlock positiveBlock = process.addBlock("recv_ty_positive");
    IRBlock negativeBlock = process.addBlock("recv_ty_negative");

    block.add(
        new IRPopType(record(node.getChs()), positiveBlock.getLabel(), negativeBlock.getLabel()));

    ep = ep.assoc(node.getTyid(), new TypeEntry(new ASTIdT(node.getTyid())));
    ep = ep.assoc(node.getTyidGen(), new TypeEntry(new ASTIdT(node.getTyidGen())));

    // Generate code for each possible polarity.
    environment().setPolarity(node.getTyid(), true);
    environment().setPolarity(node.getTyidGen(), true);
    visitBlock(positiveBlock, node.getRhs());
    environment().setPolarity(node.getTyid(), false);
    environment().setPolarity(node.getTyidGen(), false);
    visitBlock(negativeBlock, node.getRhs());
  }

  @Override
  public void visit(ASTSelect node) {
    block.add(new IRPushTag(record(node.getCh()), node.getLabelIndex()));

    // Flip if the remainder of the session type is negative.
    if (!isPositive(node.getRhsType())) {
      block.add(new IRFlip(record(node.getCh())));
    }
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

    for (int i = 0; i < node.getPars().size(); ++i) {
      linearArguments.add(new LinearArgument(record(node.getPars().get(i)), i));
    }

    for (int i = 0; i < node.getGPars().size(); ++i) {
      exponentialArguments.add(new ExponentialArgument(exponential(node.getGPars().get(i)), i));
    }

    // Determine the suffix to pick the correct process definition based on type argument polarity.
    String suffix = "";
    for (int i = 0; i < node.getTPars().size(); ++i) {
      suffix += isPositive(node.getTPars().get(i)) ? "p" : "n";
    }
    if (!suffix.isEmpty()) {
      suffix = "_" + suffix;
    }

    block.add(new IRCallProcess(node.getId() + suffix, linearArguments, exponentialArguments));
  }

  @Override
  public void visit(ASTPrintLn node) {
    GeneratedExpression expr = generateExpression(node.getExpr());
    block.add(new IRPrint(expr.getExpr(), node.withNewLine()));
    expr.freeUsedRecords(block);
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCoExpr node) {
    GeneratedExpression expr = generateExpression(node.getExpr());

    block.add(new IRPushExpression(record(node.getCh()), expr.getExpr()));
    expr.freeUsedRecords(block);
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

    expr.freeUsedRecords(thenBlock);
    expr.freeUsedRecords(elseBlock);

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
    List<InheritedExponential> inheritedExponentials = new ArrayList<>();
    Environment env = Environment.forExponential(environment(), node, inheritedExponentials);

    block.add(new IRPushExponential(record(node.getChr()), env.getName(), inheritedExponentials));
    block.add(new IRReturn(record(node.getChr())));

    IRProcess parentProcess = process;
    process =
        new IRProcess(
            true, env.recordCount(), env.exponentialCount(), countEndPoints(node.getRhs()));
    program.addProcess(env.getName(), process);
    environments.push(env);
    visitBlock(process.getEntry(), node.getRhs());
    environments.pop();
    process = parentProcess;
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
    block.add(
        new IRCallExponential(
            exponential(node.getChr()),
            record(node.getChi()),
            ASTIntoIRType.convert(ep, node.getType())));
    decExponentialRefIfUnused(node.getRhs(), node.getChr());

    // Flip to the called session if it is negative.
    if (!isPositive(node.getType())) {
      block.add(new IRFlip(record(node.getChi())));
    }
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTFwdB node) {
    block.add(new IRForwardExponential(record(node.getCh1()), exponential(node.getCh2())));
  }

  @Override
  public void visit(ASTUnfold node) {
    if (node.rec) {
      block.add(new IRPushUnfold(record(node.getCh())));

      if (!isPositive(node.getRhsType())) {
        block.add(new IRFlip(record(node.getCh())));
      }
    } else {
      block.add(new IRPopUnfold(record(node.getCh())));
    }

    node.getRhs().accept(this);
  }

  // ======================================== Utilities =========================================

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

  private Environment environment() {
    return environments.peek();
  }

  private int record(String ch) {
    return environment().record(ch);
  }

  private int exponential(String ch) {
    return environment().exponential(ch);
  }

  private boolean isExponential(String ch) {
    return environment().exponentials.containsKey(ch);
  }

  private static int countEndPoints(ASTNode node) {
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
    public void visit(ASTBang node) {}

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
      node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRecvTy node) {
      node.getRhs().accept(this);
    }
  }

  private static class GeneratedExpression {
    private final IRExpression expr;
    private final Set<Integer> usedRecords;

    public GeneratedExpression(IRExpression expr, Set<Integer> usedRecords) {
      this.expr = expr;
      this.usedRecords = usedRecords;
    }

    public IRExpression getExpr() {
      return expr;
    }

    public void freeUsedRecords(IRBlock block) {
      for (int record : usedRecords) {
        block.add(new IRFreeSession(record));
      }
    }
  }

  private GeneratedExpression generateExpression(ASTExpr expr) {
    ExpressionGenerator gen = new ExpressionGenerator();
    expr.accept(gen);
    return new GeneratedExpression(gen.ir, gen.usedRecords);
  }

  private class ExpressionGenerator extends ASTExprVisitor {
    private IRExpression ir;
    private Set<Integer> usedRecords = new HashSet<>();

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
      ir = new IRVar(record(expr.getCh()), ASTIntoIRType.convert(ep, expr.getType()));
      usedRecords.add(record(expr.getCh()));
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

  private static class Environment {
    private final String name;
    private final Environment parent;
    private final Map<String, Integer> records = new HashMap<>();
    private final Map<String, Integer> exponentials = new HashMap<>();
    private final Map<String, Boolean> polarities = new HashMap<>();

    // This function takes a process definition, and for each polarity combination of its type
    // arguments,
    // calls the given consumer with a corresponding environment and name suffix.
    public static void forEachProcessPolarity(
        ASTProcDef procDef, BiConsumer<String, Environment> forEach) {
      Environment env = new Environment(null, procDef.getId());
      for (String arg : procDef.getArgs()) {
        env.insertLinear(arg);
      }
      for (String arg : procDef.getGArgs()) {
        env.insertExponential(arg);
      }
      procDef.getRhs().accept(env.new Assigner());

      // Generate combinations of polarities.
      for (int i = 0; i < (1 << procDef.getTArgs().size()); ++i) {
        String suffix = "";
        for (int j = 0; j < procDef.getTArgs().size(); ++j) {
          String arg = procDef.getTArgs().get(j);
          boolean positive = (i & (1 << j)) != 0;
          env.setPolarity(arg, positive);
          suffix += (positive ? "p" : "n");
        }
        if (!suffix.isEmpty()) {
          suffix = "_" + suffix;
        }

        forEach.accept(suffix, env);
      }
    }

    private static Environment forExponential(
        Environment parent,
        ASTNode node,
        String name,
        String linear,
        List<InheritedExponential> outInheritedExponentials) {
      Environment env = new Environment(parent, parent.name + "_bang_" + name);
      if (linear != null) {
        env.insertLinear(linear);
      }

      // Inherit used exponentials.
      Set<String> freeNames = namesFreeIn(node);
      for (Map.Entry<String, Integer> entry : parent.exponentials.entrySet()) {
        if (freeNames.contains(entry.getKey()) && entry.getKey() != linear) {
          int index = env.insertExponential(entry.getKey());
          outInheritedExponentials.add(new InheritedExponential(entry.getValue(), index));
        }
      }

      node.accept(env.new Assigner());
      return env;
    }

    public static Environment forExponential(
        Environment parent, ASTBang node, List<InheritedExponential> outInheritedExponentials) {
      return forExponential(
          parent, node.getRhs(), node.getChr(), node.getChi(), outInheritedExponentials);
    }

    public static Environment forExponential(
        Environment parent,
        ASTPromoCoExpr node,
        List<InheritedExponential> outInheritedExponentials) {
      return forExponential(
          parent, node.getExpr(), node.getCh(), node.getCh(), outInheritedExponentials);
    }

    private Environment(Environment parent, String name) {
      this.parent = parent;
      this.name = name;
    }

    public int recordCount() {
      return records.size();
    }

    public int record(String session) {
      return records.get(session);
    }

    public int exponentialCount() {
      return exponentials.size();
    }

    public int exponential(String session) {
      return exponentials.get(session);
    }

    public boolean isPositive(String typeVar) {
      if (!polarities.containsKey(typeVar)) {
        if (parent == null) {
          throw new IllegalStateException("Type variable " + typeVar + " not found in environment");
        }
        return parent.isPositive(typeVar);
      }
      return polarities.get(typeVar);
    }

    public void setPolarity(String typeVar, boolean isPositive) {
      polarities.put(typeVar, isPositive);
    }

    public String getName() {
      return name;
    }

    private void insertLinear(String session) {
      records.put(session, records.size());
    }

    private int insertExponential(String session) {
      int index = exponentials.size();
      exponentials.put(session, index);
      return index;
    }

    // A visitor which simply traverses the AST and assigns an index to each session created in it.
    // Replication right-hand-sides are ignored.
    private class Assigner extends ASTNodeVisitor {
      @Override
      public void visit(ASTNode node) {
        throw new UnsupportedOperationException(
            "Nodes of type "
                + node.getClass().getName()
                + " are not yet supported by Environment.Assigner");
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
        insertExponential(node.getCh());
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTUnfold node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSend node) {
        insertLinear(node.getCho());
        node.getLhs().accept(this);
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
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTSendTy node) {
        node.getRhs().accept(this);
      }

      @Override
      public void visit(ASTExpr node) {}
    }
  }
}
