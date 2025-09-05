package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.function.Consumer;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.*;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.expression.IRExpression;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.*;

public class IRGenerator extends ASTNodeVisitor {
  Compiler compiler;

  private IRProgram program = new IRProgram();
  private Map<IRProcessId, ASTProcDef> procDefs = new HashMap<>();
  private Map<IRProcessId, Runnable> procGens = new HashMap<>();
  private Map<IRProcessId, IREnvironment> procEnvs = new HashMap<>();
  private Queue<IRProcessId> procUsed = new LinkedList<>();

  IRProcess process;
  IRBlock block;
  private int nextProcessGenId = 0;

  IREnvironment env;

  public static IRProgram generate(Compiler compiler, Env<EnvEntry> ep, ASTProgram ast) {
    IRGenerator gen = new IRGenerator();
    gen.compiler = compiler;

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
        gen.procUsed.add(gen.processId(procDef.getId(), new boolean[0]));
      }

      gen.forEachCombination(
          procDef,
          tArgPolarities -> {
            IRProcessId id = gen.processId(procDef.getId(), tArgPolarities);
            IRProcess process = new IRProcess(id);
            gen.env = gen.environment(procDef, ep, process, tArgPolarities);
            process.setEndPoints(gen.countEndPoints(procDef.getRhs()));
            gen.procDefs.put(id, procDef);
            gen.procGens.put(id, () -> procDef.getRhs().accept(gen));
            gen.procEnvs.put(id, gen.env);
          });
    }

    // Check that the entry process was found
    if (gen.procUsed.isEmpty()) {
      throw new IllegalArgumentException(
          "Entry process " + compiler.entryProcess.get() + " not found");
    }

    // Until all used processes have been generated, generate them
    while (!gen.procUsed.isEmpty()) {
      IRProcessId id = gen.procUsed.poll();
      if (gen.program.get(id) != null) {
        continue;
      }

      // Create empty process
      gen.env = gen.procEnvs.get(id);
      gen.process = gen.env.getProcess();
      gen.program.add(gen.process);

      // Generate process
      gen.recurse(gen.process.getEntry(), gen.procGens.get(id));
    }

    return gen.program;
  }

  private void forEachCombination(ASTProcDef procDef, Consumer<boolean[]> consumer) {
    // Generate every possible combination of type argument polarities and values
    boolean tArgPolarities[] = new boolean[procDef.getTArgs().size()];
    for (int i = 0; i < (1 << procDef.getTArgs().size()); ++i) {
      for (int j = 0; j < tArgPolarities.length; ++j) {
        tArgPolarities[j] = (i & (1 << j)) != 0;
      }
      consumer.accept(tArgPolarities);
    }
  }

  private IREnvironment environment(
      ASTProcDef procDef, Env<EnvEntry> globalEp, IRProcess process, boolean tArgPolarities[]) {
    env = new IREnvironment(process, globalEp);

    // Start by collecting type arguments
    for (int i = 0; i < procDef.getTArgs().size(); ++i) {
      String name = procDef.getTArgs().get(i);
      boolean isPositive = tArgPolarities[i];
      env = env.addType(name, isPositive);

      TypeEntry typeEntry = new TypeEntry(new ASTIdT(name));
      Env<EnvEntry> ep = env.getEp();
      ep = ep.assoc(name, typeEntry);
      ep = ep.assoc(procDef.getTArgsGen().get(i), typeEntry);
      env = env.changeEp(ep);
    }

    // Define sessions and data for each of the linear arguments
    for (int i = 0; i < procDef.getArgs().size(); ++i) {
      String name = procDef.getArgs().get(i);
      ASTType type = procDef.getArgTypes().get(i);

      IRSlotsFromASTType info = slotsFromType(type);
      boolean isPositive = env.isPositive(type);
      boolean isValue = IRValueChecker.check(compiler, env, type, isPositive);

      if (isPositive && isValue) {
        // Positive value: no need to store local data
        env = env.addSession(name);
      } else if (!isPositive && isValue) {
        // Negative value: no need for a session, just store the data
        env = env.addValue(name, info.localCombinations(), Optional.empty());
      } else {
        env = env.addArgSession(name, info.localCombinations());
      }
    }

    // Define the exponential arguments
    for (int i = 0; i < procDef.getGArgs().size(); ++i) {
      String name = procDef.getGArgs().get(i);
      IRSlotsFromASTType info = slotsFromType(new ASTWhyT(procDef.getGArgTypes().get(i)));
      env = env.addValue(name, info.combinations(), Optional.of(info.activeLocalTree));
      env = env.makeChannelExponential(name, info.activeLocalTree, true, id -> {});
    }

    return env;
  }

  // ============================ Instruction generation visit methods ============================

  @Override
  public void visit(ASTBang node) {
    IREnvironment.Channel channel = env.getChannel(node.getChr());
    IRSlotsFromASTType info = slotsFromType(node.getType());

    if (isValue(node.getType(), true)) {
      // If the exponential is a value, we generate the right-hand-side in place
      // to write directly to the channel's remote data
      env = env.alias(node.getChr(), node.getChi());
      recurse(block, node.getRhs());
    } else {
      // Create a new process for the exponential
      IRProcessId expProcessId = new IRProcessId(process.getId() + "_exp" + nextProcessGenId++);
      IRProcess expProcess = new IRProcess(expProcessId);
      expProcess.setEndPoints(countEndPoints(node.getRhs()));
      IREnvironment expEnv = new IREnvironment(expProcess, env.getEp());

      // Define the argument session for the exponential
      expEnv = expEnv.addArgSession(node.getChi(), info.localCombinations());

      // Pass all types in the current environment to the exponential process
      List<IRWriteExponential.TypeArgument> typeArguments = new ArrayList<>();
      for (int i = 0; i < process.getTypeCount(); ++i) {
        IRTypeId id = new IRTypeId(i);
        IREnvironment.Type envType = env.getType(id);
        typeArguments.add(new IRWriteExponential.TypeArgument(IRSlotTree.of(new IRVarS(id)), id));
        expEnv = expEnv.addType(envType.getName(), envType.isPositive());
      }

      // Pass captured exponentials to the exponential process as data arguments
      List<IRWriteExponential.DataArgument> dataArguments = new ArrayList<>();
      for (String name : node.getRhs().fn(new HashSet<>())) {
        if (name.equals(node.getChi())) {
          continue;
        }

        IREnvironment.Channel captured = env.getChannel(name);
        IRSlotTree valueSlots = captured.getExponentialType();

        expEnv = expEnv.addValue(name, valueSlots.combinations(), Optional.of(valueSlots));

        dataArguments.add(
            new IRWriteExponential.DataArgument(
                captured.getLocalData(),
                expEnv.getChannel(name).getLocalDataId(),
                valueSlots,
                false));
      }

      // Store the process we just defined so that it gets generated later
      procGens.put(expProcessId, () -> node.getRhs().accept(this));
      procEnvs.put(expProcessId, expEnv);
      procUsed.add(expProcessId);

      block.add(
          new IRWriteExponential(
              channel.getRemoteData(), expProcessId, typeArguments, dataArguments));
      block.add(new IRFinishSession(channel.getSessionId(), true));
    }
  }

  @Override
  public void visit(ASTCall node) {
    IREnvironment.Channel channel = env.getChannel(node.getChr());

    // Define new channel
    IRSlotsFromASTType info = slotsFromType(node.getType());
    env = env.addSession(node.getChi(), info.localCombinations());
    IREnvironment.Channel argChannel = env.getChannel(node.getChi());

    if (isValue(node.getType(), false)) {
      // TODO: if the value is a basic value (i.e., no clone/drop needed, we can just alias the
      // channels)

      // Just clone the exponential's data to the new channel
      block.add(
          new IRCloneValue(
              argChannel.getLocalData(), channel.getLocalData(), info.activeLocalTree));
    } else {
      block.add(
          new IRCallExponential(
              channel.getLocalData(), argChannel.getSessionId(), argChannel.getLocalDataId()));

      // If the called session is negative, we must jump to it
      addContinueIfNegative(argChannel.getSessionId(), node.getType());
    }

    // Recurse on the continuation
    recurse(block, node.getRhs());
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
      recurse(
          caseBlock,
          () -> {
            advanceOrReset(node.getCh(), new IRTagS(), node.getCaseType(label), false);
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
        new IRInitializeSession(
            channel.getSessionId(), negBlock.getLocation(), channel.getLocalData()));

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

    // If the type still has a continuation, we must forward two sessions to each other
    if (!isValue(negChType, false)) {
      block.add(new IRForwardSessions(neg.getSessionId(), pos.getSessionId(), true));
    } else {
      // Jump to the positive session's continuation
      block.add(new IRFinishSession(pos.getSessionId(), true));
    }
  }

  @Override
  public void visit(ASTFwdB node) {
    IREnvironment.Channel channel = env.getChannel(node.getCh1());
    IREnvironment.Channel exponential = env.getChannel(node.getCh2());

    IRSlotsFromASTType info = slotsFromType(new ASTWhyT(node.getType()));
    block.add(
        new IRCloneValue(
            channel.getRemoteData(), exponential.getLocalData(), info.activeLocalTree));
    block.add(new IRFinishSession(channel.getSessionId(), true));
  }

  @Override
  public void visit(ASTId node) {
    // Figure out the type argument properties to decide which process to call
    // Additionally, collect the type arguments we'll be passing to it
    boolean tArgPolarities[] = new boolean[node.getTPars().size()];
    List<IRCallProcess.TypeArgument> typeArguments = new ArrayList<>();

    for (int i = 0; i < node.getTPars().size(); ++i) {
      ASTType type = node.getTPars().get(i);
      IRSlotsFromASTType info = slotsFromType(type);
      tArgPolarities[i] = isPositive(type);

      IRSlotTree tree = tArgPolarities[i] ? info.activeRemoteTree : info.activeLocalTree;
      typeArguments.add(new IRCallProcess.TypeArgument(tree, new IRTypeId(i)));
    }

    // Find the process id, and mark it as used so that it gets generated
    IRProcessId processId = processId(node.getId(), tArgPolarities);
    procUsed.add(processId);
    IREnvironment processEnv = procEnvs.get(processId);
    ASTProcDef processDef = procDefs.get(processId);

    // Get a map from the process' type argument names to the types we're passing
    Map<String, ASTType> typeArgs = new HashMap<>();
    for (int i = 0; i < node.getTPars().size(); ++i) {
      ASTType type = node.getTPars().get(i);
      typeArgs.put(node.getProcTParIds().get(i), type);
    }
    env = env.withKnownTypes(typeArgs);

    // Collect the session and data arguments we'll be passing to it
    List<IRCallProcess.SessionArgument> sessionArguments = new ArrayList<>();
    List<IRCallProcess.DataArgument> dataArguments = new ArrayList<>();

    for (int i = 0; i < node.getPars().size(); ++i) {
      // Get the argument's type information
      ASTType type = node.getProcParTypes().get(i);
      try {
        type = type.unfoldType(env.getEp());
      } catch (Exception e) {
        throw new IllegalArgumentException("Error unfolding argument type: " + e.getMessage());
      }
      IRSlotsFromASTType info = slotsFromType(type);
      boolean isPositive = isPositive(type);
      boolean isValue = isValue(type, isPositive);

      // Get the arguments' channel
      IREnvironment.Channel argChannel = env.getChannel(node.getPars().get(i));
      IREnvironment.Channel targetChannel = processEnv.getChannel(processDef.getArgs().get(i));

      // If the channel is polymorphic, we need to perform translation
      IREnvironment.Channel channel;
      if (IRUsesTypeVar.check(type, typeArgs.keySet())) {
        // Create a new channel with the polymorphic type
        channel = IRPolyTranslator.translate(this, typeArgs, type, argChannel.getName());
        addContinueIfNegative(channel.getSessionId(), type);
      } else {
        channel = argChannel;
      }

      if (!isValue || isPositive) {
        // If the type is not a value, or if it's a positive value, we must pass the session
        sessionArguments.add(
            new IRCallProcess.SessionArgument(
                channel.getSessionId(), targetChannel.getSessionId(), channel.getOffset()));
      }

      if ((!isValue || !isPositive) && !info.activeLocalTree.isLeaf()) {
        // If the type is not a value, or if it's a negative value, we must pass the data
        dataArguments.add(
            new IRCallProcess.DataArgument(
                channel.getLocalData(),
                targetChannel.getLocalDataId(),
                info.activeLocalTree,
                false));
      }
    }

    for (int i = 0; i < node.getGPars().size(); ++i) {
      IREnvironment.Channel channel = env.getChannel(node.getGPars().get(i));
      IREnvironment.Channel targetChannel = processEnv.getChannel(processDef.getGArgs().get(i));
      IRSlotsFromASTType info = slotsFromType(new ASTWhyT(node.getGParTypes().get(i)));

      dataArguments.add(
          new IRCallProcess.DataArgument(
              channel.getLocalData(), targetChannel.getLocalDataId(), info.activeLocalTree, true));
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
    block.add(new IRPrint(expression(node.getExpr()), node.withNewLine()));
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
      advanceOrReset(node.getChr(), info.activeLocalTree.combinations(), node.getRhsType(), false);
    } else {
      // Bind the new session to the value received from the main session
      block.add(
          new IRBindSession(
              channel.getLocalData(), argChannel.getSessionId(), argChannel.getLocalData()));
      advanceOrReset(node.getChr(), new IRSessionS(), node.getRhsType(), false);

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
    advanceOrReset(node.getCh(), new IRTagS(), node.getRhsType(), true);

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
      advanceOrReset(
          node.getChs(), argInfo.activeRemoteTree.combinations(), node.getRhsType(), true);
      recurse(
          rhsBlock,
          () -> {
            // If the continuation is negative, we must jump to it
            addContinueIfNegative(channel.getSessionId(), node.getRhsType());
            node.getRhs().accept(this);
          });
    } else {
      // Define new session for the channel being sent
      env = env.addSession(node.getCho(), argInfo.localCombinations());
      argSession = env.getChannel(node.getCho());

      // Initialize the new session with a new closure block for the left-hand-side
      // as its continuation.
      IRBlock closureBlock = process.createBlock("send_closure");
      recurse(closureBlock, node.getLhs());
      block.add(
          new IRInitializeSession(
              argSession.getSessionId(), closureBlock.getLocation(), argSession.getLocalData()));

      block.add(new IRWriteSession(channel.getRemoteData(), argSession.getSessionId()));
      advanceOrReset(node.getChs(), new IRSessionS(), node.getRhsType(), true);

      // If the continuation is negative, we must jump to it
      addContinueIfNegative(channel.getSessionId(), node.getRhsType());

      // Recurse on the continuation
      recurse(block, node.getRhs());
    }
  }

  @Override
  public void visit(ASTUnfold node) {
    IREnvironment.Channel channel = env.getChannel(node.getCh());

    addContinue(channel.getSessionId());
    if (node.isPos()) {
      addContinueIfNegative(channel.getSessionId(), node.getRhsType());
    }
    env = env.resetChannel(node.getCh());

    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTWhy node) {
    // The only thing we do here is marking the channel as an exponential and setting the drop bit
    // The channel holds the exponential at the current offset
    // Whenever we need to access the exponential, we just read it from the channel's local data

    IRSlotsFromASTType info = slotsFromType(new ASTWhyT(node.getType()));
    env =
        env.makeChannelExponential(
            node.getCh(),
            info.activeLocalTree,
            false,
            id -> {
              block.add(new IRDeferDrop(id));
            });

    recurse(block, node.getRhs());
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
    IREnvironment.Channel channel = env.getChannel(node.getCh());
    IRExpression expression = expression(node.getExpr());
    block.add(new IRWriteExpression(channel.getRemoteData(), expression));
    block.add(new IRFinishSession(channel.getSessionId(), true));
  }

  @Override
  public void visit(ASTIf node) {
    IRBlock thenBlock = process.createBlock("if_then");
    IRBlock elseBlock = process.createBlock("if_else");
    IRExpression condition = expression(node.getExpr());

    IRBranch.Case then = new IRBranch.Case(thenBlock.getLocation(), countEndPoints(node.getThen()));
    IRBranch.Case otherwise =
        new IRBranch.Case(elseBlock.getLocation(), countEndPoints(node.getElse()));

    block.add(new IRBranchExpression(condition, then, otherwise));

    recurse(thenBlock, node.getThen());
    recurse(elseBlock, node.getElse());
  }

  @Override
  public void visit(ASTSendTy node) {
    IREnvironment.Channel channel = env.getChannel(node.getChs());

    // Get the locations where we'll be writing the necessary data
    IRDataLocation typeLoc = env.getChannel(node.getChs()).getRemoteData();
    env = env.advanceChannel(node.getChs(), IRSlotOffset.of(new IRTypeS(), new IRSessionS()));
    IRDataLocation sessionLoc = env.getChannel(node.getChs()).getRemoteData();
    env = env.advanceChannel(node.getChs(), IRSlotOffset.of(new IRSessionS(), new IRTagS()));
    IRDataLocation polarityLoc = env.getChannel(node.getChs()).getRemoteData();

    // Overwrite the previous session with a new session of the new type
    IRBlock rhsBlock = process.createBlock("sendty_rhs");
    IRSlotsFromASTType info = slotsFromType(node.getTypeRhs());
    env = env.addSession(node.getChs(), info.localCombinations());
    IREnvironment.Channel instChannel = env.getChannel(node.getChs());
    block.add(new IRInitializeSession(instChannel.getSessionId(), rhsBlock.getLocation(), instChannel.getLocalData()));

    // Create the polymorphic translator session that we'll be sending through the main channel
    Map<String, ASTType> varTypes = Map.of(node.getTypeId(), node.getType());
    env = env.withKnownTypes(varTypes);
    IREnvironment.Channel polyChannel = IRPolyTranslator.translate(this, varTypes, node.getTypeRhsNoSubst().dualCatch(env.getEp()), node.getChs());

    addContinueIfPositive(instChannel.getSessionId(), node.getTypeRhs());

    // Write the type, the translator session and the type's polarity to the main channel
    block.add(new IRWriteType(typeLoc, info.activeRemoteTree));
    block.add(new IRWriteSession(sessionLoc, polyChannel.getSessionId()));
    block.add(new IRWriteTag(polarityLoc, isPositive(node.getType()) ? 1 : 0));
    block.add(new IRFinishSession(channel.getSessionId(), true));

    // Generate the continuation
    recurse(rhsBlock, node.getRhs());
  }

  @Override
  public void visit(ASTRecvTy node) {
    // Get the locations where we'll be reading from the type and the new session
    IRDataLocation typeLoc = env.getChannel(node.getChs()).getLocalData();
    env =
        env.advanceChannel(
            node.getChs(), IRSlotOffset.of(IRSlotSequence.of(new IRTypeS()), new IRSessionS()));
    IRDataLocation sessionLoc = env.getChannel(node.getChs()).getLocalData();
    env = env.advanceChannel(node.getChs(), IRSlotOffset.of(new IRSessionS(), new IRTagS()));
    IRDataLocation polarityLoc = env.getChannel(node.getChs()).getLocalData();

    int processGenId = nextProcessGenId++;

    Consumer<Boolean> forPolarity =
        polarity -> {
          // Create a new process which will handle the new type variable
          IRProcessId polyProcessId = processId(process.getId() + "_poly" + processGenId, new boolean[] {polarity});
          IRProcess polyProcess = new IRProcess(polyProcessId);
          polyProcess.setEndPoints(countEndPoints(node.getRhs()));
          IREnvironment polyEnv = new IREnvironment(polyProcess, env.getEp());

          // Prepare lists of arguments to pass to the polymorphic process
          List<IRCallProcess.TypeArgument> typeArguments = new ArrayList<>();
          List<IRCallProcess.SessionArgument> sessionArguments = new ArrayList<>();
          List<IRCallProcess.DataArgument> dataArguments = new ArrayList<>();

          // Pass all types in the current environment to the polymorphic process
          for (int i = 0; i < process.getTypeCount(); ++i) {
            IRTypeId id = new IRTypeId(i);
            IREnvironment.Type envType = env.getType(id);
            polyEnv = polyEnv.addType(envType.getName(), envType.isPositive());
            IRTypeId newId = polyEnv.getType(envType.getName()).getId();
            typeArguments.add(new IRCallProcess.TypeArgument(IRSlotTree.of(new IRVarS(id)), newId));
          }

          // Pass the type variable received to the polymorphic process
          polyEnv = polyEnv.addType(node.getTyid(), polarity);
          Env<EnvEntry> polyEp = polyEnv.getEp();
          polyEp = polyEp.assoc(node.getTyid(), new TypeEntry(new ASTIdT(node.getTyid())));
          polyEp = polyEp.assoc(node.getTyidGen(), new TypeEntry(new ASTIdT(node.getTyid())));
          polyEp = polyEp.assoc(node.getTyidPar(), new TypeEntry(new ASTIdT(node.getTyid())));
          polyEnv = polyEnv.changeEp(polyEp);
          typeArguments.add(
              new IRCallProcess.TypeArgument(typeLoc, polyEnv.getType(node.getTyid()).getId()));

          // Define the argument session for the process
          IRSlotsFromASTType rhsInfo = IRSlotsFromASTType.compute(compiler, polyEnv, Set.of(), node.getTypeRhs());
          polyEnv = polyEnv.addArgSession(node.getChs(), rhsInfo.localCombinations());
          sessionArguments.add(
              new IRCallProcess.SessionArgument(
                  sessionLoc, polyEnv.getChannel(node.getChs()).getSessionId(), IRSlotOffset.ZERO));

          // Pass captured sessions and exponentials to the polymorphic process
          // as session and data arguments
          for (String name : node.getRhs().fn(new HashSet<>())) {
            if (name.equals(node.getChs())) {
              continue;
            }

            IREnvironment.Channel captured = env.getChannel(name);
            if (captured.isExponential()) {
              // We captured an exponential channel, we pass it as a data argument
              IRSlotTree valueSlots = captured.getExponentialType();
              polyEnv = polyEnv.addValue(name, valueSlots.combinations(), Optional.of(valueSlots));
              dataArguments.add(
                  new IRCallProcess.DataArgument(
                      captured.getLocalData(),
                      polyEnv.getChannel(name).getLocalDataId(),
                      valueSlots,
                      false));
            } else {
              // We captured a channel, we pass it as a session argument along with its data
              ASTType type = node.getFreeNameType(name);
              IRSlotsFromASTType info = IRSlotsFromASTType.compute(compiler, polyEnv, Set.of(), type);
              boolean isPositive = polyEnv.isPositive(type);
              boolean isValue = IRValueChecker.check(compiler, polyEnv, type, Optional.empty());

              polyEnv = polyEnv.addArgSession(name, info.localCombinations());
              IREnvironment.Channel sourceChannel = env.getChannel(name);
              IREnvironment.Channel targetChannel = polyEnv.getChannel(name);

              if (!isValue || isPositive) {
                // If the type is not a value, or if it's a positive value, we must pass the session
                sessionArguments.add(
                    new IRCallProcess.SessionArgument(
                        sourceChannel.getSessionId(),
                        targetChannel.getSessionId(),
                        sourceChannel.getOffset()));
              }

              if ((!isValue || !isPositive) && !info.activeLocalTree.isLeaf()) {
                // If the type is not a value, or if it's a negative value, we must pass the data
                dataArguments.add(
                    new IRCallProcess.DataArgument(
                        sourceChannel.getLocalData(),
                        targetChannel.getLocalDataId(),
                        info.activeLocalTree,
                        false));
              }
            }
          }

          // Store the process we just defined so that it gets generated later
          procGens.put(polyProcessId, () -> {
            // We need to immediately continue the received session if it's negative
            addContinueIfNegative(env.getChannel(node.getChs()).getSessionId(), node.getTypeRhs());

            node.getRhs().accept(this);
          });
          procEnvs.put(polyProcessId, polyEnv);
          procUsed.add(polyProcessId);

          // Call the polymorphic process
          block.add(
              new IRCallProcess(
                  polyProcessId, typeArguments, sessionArguments, dataArguments, true));
        };

    // Branch on the polarity tag we received, calling forPolarity with the correct value
    IRBlock positiveBlock = process.createBlock("recvty_pos");
    IRBlock negativeBlock = process.createBlock("recvty_neg");
    IRBranch.Case positiveCase = new IRBranch.Case(positiveBlock.getLocation(), 1);
    IRBranch.Case negativeCase = new IRBranch.Case(negativeBlock.getLocation(), 1);
    block.add(new IRBranchTag(polarityLoc, List.of(negativeCase, positiveCase)));

    // Generate the branches
    recurse(positiveBlock, () -> forPolarity.accept(true));
    recurse(negativeBlock, () -> forPolarity.accept(false));
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
    IREnvironment.Channel channel = env.getChannel(node.getCh());
    IRSlotsFromASTType info = slotsFromType(node.getType());
    block.add(new IRWriteScan(channel.getRemoteData(), info.slot.orElseThrow()));
    block.add(new IRFinishSession(channel.getSessionId(), true));
  }

  @Override
  public void visit(ASTSleep node) {
    block.add(new IRSleep(node.getMsecs()));
    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTUnreachable node) {
    block.add(new IRPanic("Unreachable code reached at line " + node.lineno));
  }

  @Override
  public void visit(ASTExpr node) {
    throw new UnsupportedOperationException("Expressions should not appear here");
  }

  // ======================================= Helper methods =======================================

  void addContinue(IRSessionId sessionId) {
    IRBlock contBlock = process.createBlock("continue");
    block.add(new IRContinueSession(sessionId, contBlock.getLocation()));
    block = contBlock;
  }

  void addContinueIfNegative(IRSessionId sessionId, ASTType type) {
    if (!isPositive(type)) {
      addContinue(sessionId);
    }
  }

  void addContinueIfPositive(IRSessionId sessionId, ASTType type) {
    if (isPositive(type)) {
      addContinue(sessionId);
    }
  }

  void advanceOrReset(String ch, IRSlot slot, ASTType cont, boolean advancePolarity) {
    advanceOrReset(ch, offset(slot, cont), cont, advancePolarity);
  }

  void advanceOrReset(String ch, IRSlotCombinations slot, ASTType cont, boolean advancePolarity) {
    advanceOrReset(ch, offset(slot, cont), cont, advancePolarity);
  }

  void advanceOrReset(String ch, IRSlotOffset offset, ASTType cont, boolean advancePolarity) {
    if (cont instanceof ASTRecT
        || cont instanceof ASTCoRecT
        || isPositive(cont) != advancePolarity) {
      env = env.resetChannel(ch);
    } else {
      env = env.advanceChannel(ch, offset);
    }
  }

  private boolean isPositive(ASTType type) {
    return env.isPositive(type);
  }

  private boolean isValue(ASTType type, boolean requiredPolarity) {
    return IRValueChecker.check(compiler, env, type, requiredPolarity);
  }

  private IRProcessId processId(String id, boolean tArgPolarities[]) {
    StringBuilder sb = new StringBuilder();
    sb.append(id);
    if (tArgPolarities.length > 0) {
      sb.append("_");
      for (int i = 0; i < tArgPolarities.length; ++i) {
        sb.append(tArgPolarities[i] ? "p" : "n");
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

  IRSlotsFromASTType slotsFromType(ASTType type) {
    return IRSlotsFromASTType.compute(compiler, env, Set.of(), type);
  }

  private int countEndPoints(ASTNode node) {
    return IREndPointCounter.count(compiler, env, node);
  }

  private void recurse(IRBlock block, ASTNode node) {
    recurse(block, () -> node.accept(this));
  }

  void recurse(IRBlock block, Runnable runnable) {
    IRBlock backupBlock = this.block;
    IREnvironment backupEnv = this.env;
    this.block = block;
    runnable.run();
    this.block = backupBlock;
    this.env = backupEnv;
  }
}
