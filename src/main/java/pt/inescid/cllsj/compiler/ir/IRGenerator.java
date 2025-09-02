package pt.inescid.cllsj.compiler.ir;

import java.util.Set;
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
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.IRSessionS;
import pt.inescid.cllsj.compiler.ir.slot.IRSlot;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotSequence;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRGenerator extends ASTNodeVisitor {
  private Compiler compiler;

  private IRProgram program = new IRProgram();
  private IRProcess process;
  private IRBlock block;

  private Env<EnvEntry> ep;
  private IREnvironment env;

  public static IRProgram generate(Compiler compiler, Env<EnvEntry> ep, ASTProgram ast) {
    IRGenerator gen = new IRGenerator();
    gen.compiler = compiler;
    gen.ep = ep;

    // Visit each of the processes
    for (ASTProcDef procDef : ast.getProcDefs()) {
      gen.generate(procDef);
    }

    return gen.program;
  }

  private void generate(ASTProcDef procDef) {
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
          generate(procDef, tArgPolarities, tArgValues);
        }
      } else {
        for (int m = 0; m < tArgValues.length; ++m) {
          tArgValues[m] = false;
        }
        generate(procDef, tArgPolarities, tArgValues);
      }
    }
  }

  private void generate(ASTProcDef procDef, boolean tArgPolarities[], boolean tArgValues[]) {
    // Create empty process
    IRProcessId id = processId(procDef.getId(), tArgPolarities, tArgValues);
    process = new IRProcess(id, countEndPoints(procDef.getRhs()));
    program.add(process);

    // Prepare environment for code generation
    env = new IREnvironment(process);

    for (int i = 0; i < procDef.getTArgs().size(); ++i) {
      String name = procDef.getTArgs().get(i);
      boolean isPositive = tArgPolarities[i];
      boolean isValue = tArgValues[i];
      env = env.addType(name, isPositive, isValue);
    }

    for (int i = 0; i < procDef.getArgs().size(); ++i) {
      String name = procDef.getArgs().get(i);
      IRSlotsFromASTType info = slotsFromType(procDef.getArgTypes().get(i));
      env = env.addSession(name, info.localCombinations());
    }

    for (int i = 0; i < procDef.getGArgs().size(); ++i) {
      env = env.addExponential(procDef.getGArgs().get(i));
    }

    // Visit process body
    recurse(process.getEntry(), procDef.getRhs());
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
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTClose node) {
    IRSessionId session = env.getSession(node.getCh()).getId();
    block.add(new IRFinishSession(session, true));
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
    IREnvironment.Session session = env.getSession(node.getCh());

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
        new IRInitializeSession(session.getId(), negBlock.getLocation(), session.getLocalData()));

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
    ASTType posChType;
    if (isPositive(node.getCh2Type())) {
      negCh = node.getCh1();
      posCh = node.getCh2();
      posChType = node.getCh2Type();
    } else {
      negCh = node.getCh2();
      posCh = node.getCh1();
      posChType = new ASTNotT(node.getCh2Type());
    }
    IREnvironment.Session neg = env.getSession(negCh);
    IREnvironment.Session pos = env.getSession(posCh);

    IRSlotsFromASTType info = slotsFromType(posChType);

    // The negative session holds data locally that should be written to the positive session's remote

    block.add(new IRMoveValue(pos.getRemoteData(), neg.getLocalData(), info.activeRemoteTree));

    // We need to forward sessions

    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTFwdB node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTId node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTMix node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTPrintLn node) {
    block.add(new IRPrint(expression(node.getExpr()), true));
    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTProcDef node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTProgram node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTRecv node) {
    IREnvironment.Session session = env.getSession(node.getChr());

    // Define new session for the channel being received
    IRSlotsFromASTType info = slotsFromType(node.getChiType());
    env = env.addSession(node.getChi(), info.localCombinations());
    IREnvironment.Session argSession = env.getSession(node.getChi());

    if (compiler.optimizeSendValue.get()
        && isValue(node.getChiType(), false)) {
      // If the left type is a value, we do the send value optimization
      block.add(
          new IRMoveValue(argSession.getLocalData(), session.getLocalData(), info.activeLocalTree));
      env = env.advanceSession(node.getChr(), offset(info.activeLocalTree, node.getRhsType()));
    } else {
      // Bind the new session to the value received from the main session
      block.add(
          new IRBindSession(session.getLocalData(), argSession.getId(), argSession.getLocalData()));
      env = env.advanceSession(node.getChr(), offset(new IRSessionS(), node.getRhsType()));

      // If the received session is negative, we must jump to it
      addContinueIfNegative(argSession.getId(), node.getChiType());
    }

    // Recurse on the continuation
    recurse(block, node.getRhs());
  }

  @Override
  public void visit(ASTSelect node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTSend node) {
    IREnvironment.Session session = env.getSession(node.getChs());
    IREnvironment.Session argSession;

    IRSlotsFromASTType argInfo = slotsFromType(node.getLhsType());
    if (compiler.optimizeSendValue.get()
        && isValue(node.getLhsType(), true)) {
      // We perform the send value optimization:
      // 1. we define a new session for the sent channel
      // 2. we initialize it so that it's data pointer points to the main session's data
      // 3. we jump to it immediately

      env = env.addSession(node.getCho());
      argSession = env.getSession(node.getCho());

      // Create block for the right-hand-side to run after the value has been sent
      IRBlock rhsBlock = process.createBlock("send_rhs");

      // Initialize the new session so that its data points to the main session's data
      block.add(
          new IRInitializeSession(
              argSession.getId(), rhsBlock.getLocation(), session.getRemoteData()));
      recurse(block, node.getLhs());

      // Generate the continuation
      env = env.advanceSession(node.getChs(), offset(argInfo.activeRemoteTree, node.getRhsType()));
      recurse(
          rhsBlock,
          () -> {
            // If the continuation is negative, we must jump to it
            addContinueIfNegative(session.getId(), node.getRhsType());
            node.getRhs().accept(this);
          });
    } else {
      if (compiler.optimizeSendForward.get() && node.getLhs() instanceof ASTFwd) {
        // If sending a forward, just send the forwarded session
        ASTFwd fwd = (ASTFwd) node.getLhs();
        String argCh = fwd.getCh1().equals(node.getCho()) ? fwd.getCh2() : fwd.getCh1();
        argSession = env.getSession(argCh);
      } else {
        // Define new session for the channel being sent
        env = env.addSession(node.getCho(), argInfo.localCombinations());
        argSession = env.getSession(node.getCho());

        // Initialize the new session with a new closure block for the left-hand-side
        // as its continuation. Immediately write it to the main session's remote data
        IRBlock closureBlock = process.createBlock("send_closure");
        recurse(closureBlock, node.getLhs());
        block.add(
            new IRInitializeSession(
                argSession.getId(), closureBlock.getLocation(), argSession.getLocalData()));
      }

      block.add(new IRWriteSession(session.getRemoteData(), argSession.getId()));
      env = env.advanceSession(node.getChs(), offset(new IRSessionS(), node.getRhsType()));

      // If the continuation is negative, we must jump to it
      addContinueIfNegative(session.getId(), node.getRhsType());

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
    IREnvironment.Session session = env.getSession(node.getCh());
    IRExpression expression = expression(node.getExpr());
    block.add(new IRWriteExpression(session.getRemoteData(), expression));
    block.add(new IRFinishSession(session.getId(), true));
  }

  @Override
  public void visit(ASTPromoCoExpr node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTIf node) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
    return IRValueChecker.check(env, type, requiredPolarity);
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
    return offset(IRSlotTree.of(past), remainder);
  }

  private IRSlotOffset offset(IRSlotTree past, ASTType remainder) {
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
