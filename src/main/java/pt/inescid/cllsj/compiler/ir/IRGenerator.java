package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.function.BiConsumer;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.ASTNotT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.*;

public class IRGenerator extends ASTNodeVisitor {
  private Compiler compiler;

  private IRProgram program = new IRProgram();
  private Map<IRProcessId, ASTProcDef> procDefs = new HashMap<>();
  private Map<IRProcessId, IREnvironment> procEnvs = new HashMap<>();
  private Queue<IRProcessId> procUsed = new LinkedList<>();

  private IRProcess process;
  private IRBlock block;

  private Env<EnvEntry> ep;
  private IREnvironment env;

  public static IRProgram generate(Compiler compiler, Env<EnvEntry> ep, ASTProgram ast) {
    IRGenerator gen = new IRGenerator();
    gen.compiler = compiler;
    gen.ep = ep;

    // Visit each of the processes and generate its environment
    for (ASTProcDef procDef : ast.getProcDefs()) {
      // If this is the entry process, ensure it has no arguments
      if (procDef.getId().equals(compiler.entryProcess.get())) {
        if (procDef.getTArgs().size() > 0) {
          throw new IllegalArgumentException("Entry process cannot have type arguments");
        }
        if (procDef.getGArgs().size() > 0) {
          throw new IllegalArgumentException("Entry process cannot have exponential arguments");
        }
        if (procDef.getArgs().size() > 0) {
          throw new IllegalArgumentException("Entry process cannot have linear arguments");
        }
        gen.procUsed.add(gen.processId(procDef.getId(), new boolean[0], new boolean[0]));
      }

      gen.forEachCombination(procDef, (tArgPolarities, tArgValues) -> {
        IRProcessId id = gen.processId(procDef.getId(), tArgPolarities, tArgValues);
        IRProcess process = new IRProcess(id, gen.countEndPoints(procDef.getRhs()));
        IREnvironment env = gen.environment(procDef, process, tArgPolarities, tArgValues);
        gen.procDefs.put(id, procDef);
        gen.procEnvs.put(id, env);
      });
    }

    // Check that the entry process was found
    if (gen.procUsed.isEmpty()) {
      throw new IllegalArgumentException("Entry process " + compiler.entryProcess.get() + " not found");
    }

    // Until all used processes have been generated, generate them
    while (!gen.procUsed.isEmpty()) {
      IRProcessId id = gen.procUsed.poll();
      if (gen.program.get(id) != null) {
        continue;
      }

      // Create empty process
      ASTProcDef procDef = gen.procDefs.get(id);
      gen.env = gen.procEnvs.get(id);
      gen.process = gen.env.getProcess();
      gen.program.add(gen.process);

      // Visit process body
      gen.recurse(gen.process.getEntry(), procDef.getRhs());
    }

    return gen.program;
  }

  private void forEachCombination(ASTProcDef procDef, BiConsumer<boolean[], boolean[]> consumer) {
    // Generate every possible combination of type argument polarities and values
    boolean tArgPolarities[] = new boolean[procDef.getTArgs().size()];
    boolean tArgValues[] = new boolean[procDef.getTArgs().size()];
    for (int i = 0; i < (1 << procDef.getTArgs().size()); ++i) {
      for (int j = 0; j < tArgPolarities.length; ++j) {
        tArgPolarities[j] = (i & (1 << j)) != 0;
      }
      if (compiler.optimizeSendValue.get()) {
        for (int n = 0; n < (1 << procDef.getTArgs().size()); ++n) {
          for (int m = 0; m < tArgValues.length; ++m) {
            tArgValues[m] = (n & (1 << m)) != 0;
          }
          consumer.accept(tArgPolarities, tArgValues);
        }
      } else {
        for (int m = 0; m < tArgValues.length; ++m) {
          tArgValues[m] = false;
        }
        consumer.accept(tArgPolarities, tArgValues);
      }
    }
  }

  private IREnvironment environment(ASTProcDef procDef, IRProcess process, boolean tArgPolarities[], boolean tArgValues[]) {
    IREnvironment env = new IREnvironment(process);

    // Start by collecting type arguments
    for (int i = 0; i < procDef.getTArgs().size(); ++i) {
      String name = procDef.getTArgs().get(i);
      boolean isPositive = tArgPolarities[i];
      boolean isValue = tArgValues[i];
      env = env.addType(name, isPositive, isValue);
    }

    // Define sessions and data for each of the linear arguments
    for (int i = 0; i < procDef.getArgs().size(); ++i) {
      String name = procDef.getArgs().get(i);
      ASTType type = procDef.getArgTypes().get(i);

      IRSlotsFromASTType info = slotsFromType(type);
      boolean isPositive = env.isPositive(ep, type);
      boolean isValue = IRValueChecker.check(ep, env, type, isPositive);

      if (isPositive && isValue) {
        // Positive value: no need to store local data
        env = env.addSession(name);
      } else if (!isPositive && isValue) {
        // Negative value: no need for a session, just store the data
        env = env.addValue(name, info.localCombinations());
      } else {
        env = env.addArgSession(name, info.localCombinations());
      }
    }

    // Define the exponential arguments
    for (int i = 0; i < procDef.getGArgs().size(); ++i) {
      env = env.addExponential(procDef.getGArgs().get(i));
    }

    return env;
  }

  // ============================ Instruction generation visit methods ============================

  @Override
  public void visit(ASTBang node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCall node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCase node) {
    IREnvironment.Channel channel = env.getChannel(node.getCh());

    // Generate a block for each case
    List<IRBranch.Case> cases = new ArrayList<>();
    for (int i = 0; i < node.getCaseCount(); ++i) {
      String label = node.getCaseLabelFromIndex(i);
      ASTNode c = node.getCase(label);
      IRBlock caseBlock = process.createBlock("case_" + label.substring(1).toLowerCase());
      cases.add(new IRBranch.Case(caseBlock.getLocation(), countEndPoints(c)));

      // Recurse on the case
      recurse(caseBlock, () -> {
        env = env.advanceChannel(node.getCh(), offset(new IRTagS(), node.getCaseType(label)));
        c.accept(this);
      });
    }

    block.add(new IRBranchTag(channel.getLocalData(), cases));
  }

  @Override
  public void visit(ASTClose node) {
    IRSessionId channel = env.getChannel(node.getCh()).getSessionId();
    block.add(new IRFinishSession(channel, true));
  }

  @Override
  public void visit(ASTCoClose node) {
    node.getRhs().accept(this);
  }

  @Override
  public void visit(ASTCut node) {
    // Define new session
    IRSlotsFromASTType info = slotsFromType(node.getChType());
    env = env.addSession(node.getCh(), info.combinations());
    IREnvironment.Channel channel = env.getChannel(node.getCh());

    // Find which side is the negative one and create a block for it
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
    IRBlock negBlock = process.createBlock(negLabel);

    // Initialize the new session with the negative block as its continuation
    block.add(
        new IRInitializeSession(channel.getSessionId(), negBlock.getLocation(), channel.getLocalData()));

    // Recurse on both sides
    recurse(block, pos);
    recurse(negBlock, neg);
  }

  @Override
  public void visit(ASTEmpty node) {
    block.add(new IRPopTask(true));
  }

  @Override
  public void visit(ASTFwd node) {
    // Get the negative and positive sessions
    String negCh, posCh;
    ASTType negChType;
    if (isPositive(node.getCh2Type())) {
      negCh = node.getCh1();
      posCh = node.getCh2();
      negChType = new ASTNotT(node.getCh2Type());
    } else {
      negCh = node.getCh2();
      posCh = node.getCh1();
      negChType = node.getCh2Type();
    }
    IREnvironment.Channel neg = env.getChannel(negCh);
    IREnvironment.Channel pos = env.getChannel(posCh);

    IRSlotsFromASTType info = slotsFromType(negChType);

    // Move data stored on the negative session's local data to the positive session's remote data
    block.add(new IRMoveValue(pos.getRemoteData(), neg.getLocalData(), info.activeLocalTree));

    // If the type still has a continuation, we must tie the two sessions together
    if (!isValue(negChType, false)) {
      block.add(new IRTieSessions(pos.getSessionId(), neg.getSessionId()));
    }

    // Jump to the positive session's continuation
    block.add(new IRFinishSession(pos.getSessionId(), true));
  }

  @Override
  public void visit(ASTFwdB node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTId node) {
    // Figure out the type argument properties to decide which process to call
    // Additionally, collect the type arguments we'll be passing to it
    boolean tArgPolarities[] = new boolean[node.getTPars().size()];
    boolean tArgValues[] = new boolean[node.getTPars().size()];
    List<IRCallProcess.TypeArgument> typeArguments = new ArrayList<>();

    for (int i = 0; i < node.getTPars().size(); ++i) {
      ASTType type = node.getTPars().get(i);
      IRSlotsFromASTType info = slotsFromType(type);

      tArgPolarities[i] = isPositive(type);
      tArgValues[i] = isValue(type, tArgPolarities[i]);
      
      IRSlotTree tree = tArgPolarities[i] ? info.activeRemoteTree : info.activeLocalTree;
      typeArguments.add(new IRCallProcess.TypeArgument(tree, new IRTypeId(i)));
    }

    // Find the process id, and mark it as used so that it gets generated
    IRProcessId processId = processId(node.getId(), tArgPolarities, tArgValues);
    procUsed.add(processId);
    IREnvironment processEnv = procEnvs.get(processId);
    ASTProcDef processDef = procDefs.get(processId);

    // Collect the session and data arguments we'll be passing to it
    List<IRCallProcess.SessionArgument> sessionArguments = new ArrayList<>();
    List<IRCallProcess.DataArgument> dataArguments = new ArrayList<>();

    for (int i = 0; i < node.getPars().size(); ++i) {
      // Get the argument's type information
      ASTType type = node.getParTypes().get(i);
      IRSlotsFromASTType info = slotsFromType(type);
      boolean isPositive = isPositive(type);
      boolean isValue = isValue(type, isPositive);

      // Get the arguments' channel
      IREnvironment.Channel channel = env.getChannel(node.getPars().get(i));
      IREnvironment.Channel targetChannel = processEnv.getChannel(processDef.getArgs().get(i));

      if (!isValue || isPositive) {
        // If the type is not a value, or if it's a positive value, we must pass the session
        sessionArguments.add(new IRCallProcess.SessionArgument(channel.getSessionId(), targetChannel.getSessionId(), channel.getOffset()));
      }

      if ((!isValue || !isPositive) && !info.activeLocalTree.isLeaf()) {
        // If the type is not a value, or if it's a negative value, we must pass the data
        dataArguments.add(new IRCallProcess.DataArgument(channel.getLocalData(), targetChannel.getLocalDataId(), info.activeLocalTree));
      }
    }

    for (int i = 0; i < node.getGPars().size(); ++i) {
      throw new UnsupportedOperationException("Exponential parameters not supported yet");
    }

    // Actually call the process
    block.add(new IRCallProcess(processId, typeArguments, sessionArguments, dataArguments, true));
  }

  @Override
  public void visit(ASTMix node) {
    IRBlock rhsBlock = process.createBlock("mix_rhs");
    block.add(new IRPushTask(rhsBlock.getLocation()));
    recurse(block, node.getLhs());
    recurse(rhsBlock, node.getRhs());
  }

  @Override
  public void visit(ASTPrintLn node) {
    block.add(new IRPrint(expression(node.getExpr()), true));
    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTProcDef node) {}

  @Override
  public void visit(ASTProgram node) {}

  @Override
  public void visit(ASTRecv node) {
    IREnvironment.Channel channel = env.getChannel(node.getChr());

    // Define new session for the channel being received
    IRSlotsFromASTType info = slotsFromType(node.getChiType());
    env = env.addSession(node.getChi(), info.localCombinations());
    IREnvironment.Channel argChannel = env.getChannel(node.getChi());

    if (compiler.optimizeSendValue.get() && isValue(node.getChiType(), false)) {
      // If the left type is a value, we do the send value optimization
      block.add(
          new IRMoveValue(argChannel.getLocalData(), channel.getLocalData(), info.activeLocalTree));
      env =
          env.advanceChannel(
              node.getChr(), offset(info.activeLocalTree.combinations(), node.getRhsType()));
    } else {
      // Bind the new session to the value received from the main session
      block.add(
          new IRBindSession(channel.getLocalData(), argChannel.getSessionId(), argChannel.getLocalData()));
      env = env.advanceChannel(node.getChr(), offset(new IRSessionS(), node.getRhsType()));

      // If the received session is negative, we must jump to it
      addContinueIfNegative(argChannel.getSessionId(), node.getChiType());
    }

    // Recurse on the continuation
    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTSelect node) {
    IREnvironment.Channel channel = env.getChannel(node.getCh());
    
    // Write the tag to the session's remote data
    block.add(new IRWriteTag(channel.getRemoteData(), node.getLabelIndex()));
    env = env.advanceChannel(node.getCh(), offset(new IRTagS(), node.getRhsType()));

    // If the continuation is negative, we must jump to it
    addContinueIfNegative(channel.getSessionId(), node.getRhsType());

    // Recurse on the continuation
    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTSend node) {
    IREnvironment.Channel channel = env.getChannel(node.getChs());
    IREnvironment.Channel argSession;

    IRSlotsFromASTType argInfo = slotsFromType(node.getLhsType());
    if (compiler.optimizeSendValue.get() && isValue(node.getLhsType(), true)) {
      // We perform the send value optimization:
      // 1. we define a new session for the sent channel
      // 2. we initialize it so that it's data pointer points to the main session's data
      // 3. we jump to it immediately

      env = env.addSession(node.getCho());
      argSession = env.getChannel(node.getCho());

      // Create block for the right-hand-side to run after the value has been sent
      IRBlock rhsBlock = process.createBlock("send_rhs");

      // Initialize the new session so that its data points to the main session's data
      block.add(
          new IRInitializeSession(
              argSession.getSessionId(), rhsBlock.getLocation(), channel.getRemoteData()));
      recurse(block, node.getLhs());

      // Generate the continuation
      env =
          env.advanceChannel(
              node.getChs(), offset(argInfo.activeRemoteTree.combinations(), node.getRhsType()));
      recurse(
          rhsBlock,
          () -> {
            // If the continuation is negative, we must jump to it
            addContinueIfNegative(channel.getSessionId(), node.getRhsType());
            node.getRhs().accept(this);
          });
    } else {
      if (compiler.optimizeSendForward.get() && node.getLhs() instanceof ASTFwd) {
        // If sending a forward, just send the forwarded session
        ASTFwd fwd = (ASTFwd) node.getLhs();
        String argCh = fwd.getCh1().equals(node.getCho()) ? fwd.getCh2() : fwd.getCh1();
        argSession = env.getChannel(argCh);
      } else {
        // Define new session for the channel being sent
        env = env.addSession(node.getCho(), argInfo.localCombinations());
        argSession = env.getChannel(node.getCho());

        // Initialize the new session with a new closure block for the left-hand-side
        // as its continuation. Immediately write it to the main session's remote data
        IRBlock closureBlock = process.createBlock("send_closure");
        recurse(closureBlock, node.getLhs());
        block.add(
            new IRInitializeSession(
                argSession.getSessionId(), closureBlock.getLocation(), argSession.getLocalData()));
      }

      block.add(new IRWriteSession(channel.getRemoteData(), argSession.getSessionId()));
      env = env.advanceChannel(node.getChs(), offset(new IRSessionS(), node.getRhsType()));

      // If the continuation is negative, we must jump to it
      addContinueIfNegative(channel.getSessionId(), node.getRhsType());

      // Recurse on the continuation
      recurse(block, node.getRhs());
    }
  }

  @Override
  public void visit(ASTUnfold node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTWhy node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCoExpr node) {
    IREnvironment.Channel channel = env.getChannel(node.getCh());
    IRExpression expression = expression(node.getExpr());
    block.add(new IRWriteExpression(channel.getRemoteData(), expression));
    block.add(new IRFinishSession(channel.getSessionId(), true));
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTIf node) {
    IRBlock thenBlock = process.createBlock("if_then");
    IRBlock elseBlock = process.createBlock("if_else");
    IRExpression condition = expression(node.getExpr());

    IRBranch.Case then = new IRBranch.Case(thenBlock.getLocation(), countEndPoints(node.getThen()));
    IRBranch.Case otherwise = new IRBranch.Case(elseBlock.getLocation(), countEndPoints(node.getElse()));

    block.add(new IRBranchExpression(condition, then, otherwise));

    recurse(thenBlock, node.getThen());
    recurse(elseBlock, node.getElse());
  }

  @Override
  public void visit(ASTSendTy node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTRecvTy node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTAffine node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTUse node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTDiscard node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCell node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTPut node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTTake node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTRelease node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTShare node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTShareL node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTShareR node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTScan node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTSleep node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTUnreachable node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTExpr node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  // ======================================= Helper methods =======================================

  private void addContinue(IRSessionId sessionId) {
    IRBlock contBlock = process.createBlock("continue");
    block.add(new IRContinueSession(sessionId, contBlock.getLocation()));
    block = contBlock;
  }

  private void addContinueIfNegative(IRSessionId sessionId, ASTType type) {
    if (!isPositive(type)) {
      addContinue(sessionId);
    }
  }

  private boolean isPositive(ASTType type) {
    return env.isPositive(ep, type);
  }

  private boolean isValue(ASTType type, boolean requiredPolarity) {
    return IRValueChecker.check(ep, env, type, requiredPolarity);
  }

  private IRProcessId processId(String id, boolean tArgPolarities[], boolean tArgValues[]) {
    StringBuilder sb = new StringBuilder();
    sb.append(id);
    if (tArgPolarities.length > 0) {
      sb.append("_");
      for (int i = 0; i < tArgPolarities.length; ++i) {
        sb.append(tArgPolarities[i] ? "p" : "n");
        sb.append(tArgValues[i] ? "v" : "s");
      }
    }
    return new IRProcessId(sb.toString());
  }

  private IRExpression expression(ASTExpr expr) {
    return IRExpressionGenerator.generate(
        env,
        expr,
        type ->
            slotsFromType(type)
                .slot
                .orElseThrow(
                    () ->
                        new UnsupportedOperationException("Types in expressions must have slots")));
  }

  private IRSlotOffset offset(IRSlot past, ASTType remainder) {
    return offset(IRSlotCombinations.of(past), remainder);
  }

  private IRSlotOffset offset(IRSlotCombinations past, ASTType remainder) {
    return new IRSlotOffset(past, slotsFromType(remainder).slot);
  }

  private IRSlotsFromASTType slotsFromType(ASTType type) {
    return IRSlotsFromASTType.compute(compiler, ep, env, Set.of(), type);
  }

  private int countEndPoints(ASTNode node) {
    return IREndPointCounter.count(compiler, node);
  }

  private void recurse(IRBlock block, ASTNode node) {
    recurse(block, () -> node.accept(this));
  }

  private void recurse(IRBlock block, Runnable runnable) {
    IRBlock backupBlock = this.block;
    IREnvironment backupEnv = this.env;
    this.block = block;
    runnable.run();
    this.block = backupBlock;
    this.env = backupEnv;
  }
}
