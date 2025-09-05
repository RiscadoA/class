package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.*;

// Generates code for translating a polymorphic variable to its instantiated counterpart
// and vice-versa.
public class IRPolyTranslator extends ASTTypeVisitor {
  private IRGenerator gen;
  private Map<String, ASTType> varTypes;
  private Map<String, IRSlotsFromASTType> varSlots;

  private static int nextNameId = 0;

  private Map<String, IRCodeLocation> recursionLabels = new HashMap<>();
  private Map<String, IRCodeLocation> coRecursionLabels = new HashMap<>();
  private String poly;
  private String inst;

  // Returns the name of the new channel created for the polymorphic side
  public static IREnvironment.Channel translate(
      IRGenerator gen, Map<String, ASTType> varTypes, ASTType polyType, String inst) {
    String poly = newChannelName(inst);

    gen.env = gen.env.addSession(poly, gen.slotsFromType(polyType).combinations());

    IREnvironment.Channel polyChannel = gen.env.getChannel(poly);
    IRBlock contBlock = gen.process.createBlock("poly_cont");

    gen.block.add(
        new IRInitializeSession(
            polyChannel.getSessionId(), contBlock.getLocation(), polyChannel.getLocalData()));

    gen.recurse(
        contBlock,
        () -> {
          translate(gen, varTypes, polyType, poly, inst);
        });

    return polyChannel;
  }

  public static void translate(
      IRGenerator gen, Map<String, ASTType> varTypes, ASTType polyType, String poly, String inst) {
    IRPolyTranslator translator = new IRPolyTranslator();
    translator.gen = gen;
    translator.varTypes = varTypes;
    translator.varSlots = new HashMap<>();
    for (Map.Entry<String, ASTType> entry : varTypes.entrySet()) {
      translator.varSlots.put(entry.getKey(), gen.slotsFromType(entry.getValue()));
    }
    translator.poly = poly;
    translator.inst = inst;
    translator.recurse(polyType);
  }

  private void recurse(ASTType polyType) {
    if (isPolymorphic(polyType)) {
      polyType.accept(this);
    } else {
      // If the type is not polymorphic, we just forward the sessions
      String neg, pos;
      ASTType negChType;
      if (isPositive(polyType)) {
        pos = inst;
        neg = poly;
        negChType = new ASTNotT(polyType);
      } else {
        pos = poly;
        neg = inst;
        negChType = polyType;
      }

      IRSlotsFromASTType info = polySlotsFromType(negChType);

      // Move data stored on the negative session's local data to the positive session's remote data
      gen.block.add(new IRMoveValue(remoteData(pos), localData(neg), info.activeRemoteTree));

      // If the type still has a continuation, we must forward two sessions to each other
      if (!isValuePoly(negChType, false)) {
        gen.block.add(new IRForwardSessions(sessionId(neg), sessionId(pos), true));
      } else {
        // Jump to the positive session's continuation
        gen.block.add(new IRFinishSession(sessionId(pos), true));
      }
    }
  }

  private void recurse(String polyChannel, String instChannel, ASTType polyType) {
    String oldPolyChannel = this.poly;
    String oldInstChannel = this.inst;
    this.poly = polyChannel;
    this.inst = instChannel;
    recurse(polyType);
    this.poly = oldPolyChannel;
    this.inst = oldInstChannel;
  }

  @Override
  public void visit(ASTOneT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTBotT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTintT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTCointT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTLintT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTLCointT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTLboolT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTCoLboolT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTLstringT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTCoLstringT type) {
    throw new UnsupportedOperationException(
        "Not polymorphic, should be caught by the recurse method");
  }

  @Override
  public void visit(ASTBangT type) {
    // On the polymorphic side, we have a channel of type !A(X)
    // On the instantiated side, we have a channel of type ?~A(I) where I is the instantiated type

    // We must create a new exponential with a new process which will:
    // - capture the exponential received from the instantiated side
    // - if A(I) is not a positive value, this isn't needed, we just access it directly:
    //    - the process calls the exponential to produce a value
    // - the process code translates this value to the polymorphic value (recursion)

    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTWhyT type) {
    // On the polymorphic side, we have a channel of type ?A(X)
    // On the instantiated side, we have a channel of type !~A(I) where I is the instantiated type

    // If A(I) is a negative value, we call the polymorphic exponential and translate the result
    // Otherwise, we must create a new exponential with a new process which will:
    // - capture the exponential received from the polymorphic side
    // - the process code translates the value to the instantiated value (recursion)

    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTOfferT type) {
    // Here the instantiated side sends a tag to be received by the polymorphic side

    addMove(poly, inst, new IRTagS());

    List<IRBranch.Case> cases = new ArrayList<>();

    for (int i = 0; i < type.getcases().size(); ++i) {
      String label = type.getLabel(i);
      ASTType caseType = type.getCaseType(label);

      IRBlock caseBlock = gen.process.createBlock("poly_offer_" + label.substring(1).toLowerCase());
      cases.add(new IRBranch.Case(caseBlock.getLocation(), 0));
      gen.recurse(
          caseBlock,
          () -> {
            advanceOrResetBoth(new IRTagS(), caseType, true);

            // The poly channel was positive, and the instantiated channel was negative
            // If the poly type becomes negative, we must jump to it
            gen.addContinueIfNegative(sessionId(poly), caseType);

            recurse(caseType);
          });
    }

    gen.block.add(new IRBranchTag(localData(inst), cases));
  }

  @Override
  public void visit(ASTCaseT type) {
    // Here the polymorphic side sends a tag to be received by the instantiated side

    addMove(inst, poly, new IRTagS());

    List<IRBranch.Case> cases = new ArrayList<>();

    for (int i = 0; i < type.getcases().size(); ++i) {
      String label = type.getLabel(i);
      ASTType caseType = type.getCaseType(label);

      IRBlock caseBlock = gen.process.createBlock("poly_case_" + label.substring(1).toLowerCase());
      cases.add(new IRBranch.Case(caseBlock.getLocation(), 0));
      gen.recurse(
          caseBlock,
          () -> {
            advanceOrResetBoth(new IRTagS(), caseType, false);

            // The poly channel was negative, and the instantiated channel was positive
            // If the instantiated type becomes negative, we must jump to it
            // The instantiated channel becomes negative if the poly type becomes positive
            gen.addContinueIfPositive(sessionId(inst), caseType);

            recurse(caseType);
          });
    }

    gen.block.add(new IRBranchTag(localData(inst), cases));
  }

  @Override
  public void visit(ASTRecT type) {
    // Create a new block, as we may jump back here later due to recursion
    IRBlock recBlock = gen.process.createBlock("poly_rec");
    gen.block.add(new IRJump(recBlock.getLocation()));

    Map<String, IRCodeLocation> recursionLabels = this.recursionLabels;
    gen.recurse(
        recBlock,
        () -> {
          this.recursionLabels = new HashMap<>(this.recursionLabels);
          this.recursionLabels.put(type.getid(), recBlock.getLocation());

          gen.env =
              gen.env.changeEp(
                  gen.env.getEp().assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))));

          // It is an unfold, so we must jump to both channels
          gen.addContinue(sessionId(poly));
          gen.addContinue(sessionId(inst));

          // The positive side, polyChannel, will need to be continued if the continuation is
          // negative
          gen.addContinueIfNegative(sessionId(poly), type.getin());

          gen.env.resetChannel(poly);
          gen.env.resetChannel(inst);

          recurse(type.getin());
        });
    this.recursionLabels = recursionLabels;
  }

  @Override
  public void visit(ASTCoRecT type) {
    // Create a new block, as we may jump back here later due to recursion
    IRBlock recBlock = gen.process.createBlock("poly_co_rec");
    gen.block.add(new IRJump(recBlock.getLocation()));

    Map<String, IRCodeLocation> coRecursionLabels = this.coRecursionLabels;
    gen.recurse(
        recBlock,
        () -> {
          this.coRecursionLabels = new HashMap<>(this.coRecursionLabels);
          this.coRecursionLabels.put(type.getid(), recBlock.getLocation());

          gen.env =
              gen.env.changeEp(
                  gen.env.getEp().assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid()))));

          // It is an unfold, so we must jump to both channels
          gen.addContinue(sessionId(poly));
          gen.addContinue(sessionId(inst));

          // The positive side, instChannel, will need to be continued if its continuation is
          // negative
          // Since the type we have is the dual, we continue if the poly type is positive
          gen.addContinueIfPositive(sessionId(inst), type.getin());

          gen.env.resetChannel(poly);
          gen.env.resetChannel(inst);

          recurse(type.getin());
        });
    this.coRecursionLabels = coRecursionLabels;
  }

  @Override
  public void visit(ASTRecvT type) {
    if (isPolymorphic(type.getlhs())) {
      // The left-hand-side of the recv is polymorphic.
      // Thus, the polymorphic side expects to receive a polymorphic session
      // We must perform translation of the closure or value received from the instantiated side

      // Define the new polymorphic session we'll send to the polymorphic side
      String sentPoly = newChannelName(poly);
      gen.env = gen.env.addSession(sentPoly, polySlotsFromType(type.getlhs()).remoteCombinations());
      IRBlock closureBlock = gen.process.createBlock("poly_recv_closure");
      gen.block.add(
          new IRInitializeSession(
              sessionId(sentPoly), closureBlock.getLocation(), localData(sentPoly)));

      if (gen.compiler.optimizeSendValue.get() && isValueInst(type.getlhs(), false)) {
        // If the instantiated side has sent a value, we translate it inside the closure
        gen.recurse(closureBlock, () -> recurse(sentPoly, inst, type.getlhs()));

        IRSlotsFromASTType instInfo = instSlotsFromType(type.getlhs());
        advanceOrResetInst(instInfo.activeTree().combinations(), type.getlhs(), false);
      } else {
        // Receive the new session from the instantiated side
        String recvInst = newChannelName(inst);
        gen.env =
            gen.env.addSession(recvInst, instSlotsFromType(type.getlhs()).localCombinations());
        gen.block.add(new IRBindSession(localData(inst), sessionId(recvInst), localData(recvInst)));
        advanceOrResetInst(new IRSessionS(), type.getlhs(), false);

        // Generate the closure code which translates the received instantiated session
        gen.recurse(
            closureBlock,
            () -> {
              // If the received session is negative, we must pass control to it first
              gen.addContinueIfNegative(sessionId(recvInst), type.getlhs());
              recurse(sentPoly, recvInst, type.getlhs());
            });
      }

      // Write the new polymorphic session to the polymorphic side
      gen.block.add(new IRWriteSession(remoteData(poly), sessionId(sentPoly)));
      advanceOrResetPoly(new IRSessionS(), type.getrhs(), false);

      // If the polarity changes, we pass control to the side side
      gen.addContinueIfPositive(sessionId(poly), type.getrhs());
    } else {
      // If the left-hand-side is not polymorphic, we can just forward it
      if (gen.compiler.optimizeSendValue.get() && isValuePoly(type.getlhs(), false)) {
        IRSlotsFromASTType info = polySlotsFromType(type.getlhs());
        addMove(poly, inst, info.activeTree());
        bothAdvanceOrReset(info.activeTree().combinations(), type.getrhs(), false);
      } else {
        addMove(poly, inst, new IRSessionS());
        advanceOrResetBoth(new IRSessionS(), type.getrhs(), false);
      }
    }

    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTSendT type) {
    if (isPolymorphic(type.getlhs())) {
      // The left-hand-side of the send is polymorphic.
      // Thus, the polymorphic side will send a polymorphic session
      // We must perform translation of the closure to an instantiated session or value

      // Receive the new session from the polymorphic side
      String recvPoly = newChannelName(poly);
      gen.env = gen.env.addSession(recvPoly, polySlotsFromType(type.getlhs()).localCombinations());
      gen.block.add(new IRBindSession(localData(poly), sessionId(recvPoly), localData(recvPoly)));
      advanceOrResetPoly(new IRSessionS(), type.getlhs(), true);

      // If the received session is positive, we must pass control to it first
      gen.addContinueIfPositive(sessionId(recvPoly), type.getlhs());

      if (gen.compiler.optimizeSendValue.get() && isValueInst(type.getlhs(), true)) {
        // If the instantiated side expects a value, we must translate it now
        // We thus initialize a new session so that its data points to the main instantiated channel
        IRBlock rhsBlock = gen.process.createBlock("poly_send_cont");
        String sentInst = newChannelName(inst);
        gen.env = gen.env.addSession(sentInst);
        gen.block.add(
            new IRInitializeSession(sessionId(sentInst), rhsBlock.getLocation(), remoteData(inst)));

        // Generate the code which translates the received polymorphic session to the instantiated
        // value
        gen.recurse(gen.block, () -> recurse(recvPoly, sentInst, type.getlhs()));

        // We can now continue with the right-hand-side
        gen.block = rhsBlock;
        IRSlotsFromASTType instInfo = instSlotsFromType(type.getlhs());
        advanceOrResetInst(instInfo.activeTree().combinations(), type.getlhs(), true);
      } else {
        // Otherwise, we must create a new instantiated session which translates the
        // received polymorphic session
        String sentInst = newChannelName(inst);
        gen.env =
            gen.env.addSession(sentInst, instSlotsFromType(type.getlhs()).remoteCombinations());
        IRBlock closureBlock = gen.process.createBlock("poly_send_closure");
        gen.block.add(
            new IRInitializeSession(
                sessionId(sentInst), closureBlock.getLocation(), localData(sentInst)));

        // Generate the closure code which translates the received polymorphic session
        gen.recurse(closureBlock, () -> recurse(recvPoly, sentInst, type.getlhs()));

        // Write the new instantiated session to the instantiated side
        gen.block.add(new IRWriteSession(remoteData(inst), sessionId(sentInst)));
        advanceOrResetInst(new IRSessionS(), type.getrhs(), true);
      }
    } else {
      // If the left-hand-side is not polymorphic, we can just forward it
      if (gen.compiler.optimizeSendValue.get() && isValuePoly(type.getlhs(), true)) {
        IRSlotsFromASTType info = polySlotsFromType(type.getlhs());
        addMove(inst, poly, info.activeTree());
        bothAdvanceOrReset(info.activeTree().combinations(), type.getrhs(), true);
      } else {
        addMove(inst, poly, new IRSessionS());
        advanceOrResetBoth(new IRSessionS(), type.getrhs(), true);
      }
    }

    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTSendTT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTRecvTT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTIdT type) {
    String source, target;

    if (isPositive(type)) {
      source = poly;
      target = inst;
    } else {
      source = inst;
      target = poly;
    }

    if (varTypes.containsKey(type.getid())) {
      addMove(target, source, varSlots.get(type.getid()).activeTree());
      gen.block.add(new IRFinishSession(sessionId(target), true));
    } else if (recursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(recursionLabels.get(type.getid())));
    } else if (coRecursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(coRecursionLabels.get(type.getid())));
    } else {
      throw new UnsupportedOperationException(
          "Not polymorphic, should be caught by the recurse method");
    }
  }

  @Override
  public void visit(ASTNotT notType) {
    if (!(notType.getin() instanceof ASTIdT)) {
      throw new IllegalArgumentException(
          "The typechecker should guarantee nots never appear around non-var types");
    }
    ASTIdT type = (ASTIdT) notType.getin();

    String source, target;
    if (isPositive(type)) {
      source = inst;
      target = poly;
    } else {
      source = poly;
      target = inst;
    }

    if (varTypes.containsKey(type.getid())) {
      addMove(target, source, varSlots.get(type.getid()).activeTree());
      gen.block.add(new IRFinishSession(sessionId(target), true));
    } else if (recursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(recursionLabels.get(type.getid())));
    } else if (coRecursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(coRecursionLabels.get(type.getid())));
    } else {
      throw new UnsupportedOperationException(
          "Not polymorphic, should be caught by the recurse method");
    }
  }

  @Override
  public void visit(ASTAffineT type) {
    throw new UnsupportedOperationException("Should no longer appear at this stage");
  }

  @Override
  public void visit(ASTCoAffineT type) {
    throw new UnsupportedOperationException("Should no longer appear at this stage");
  }

  @Override
  public void visit(ASTCellT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTUsageT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTCellLT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTUsageLT type) {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  private IRSessionId sessionId(String name) {
    return gen.env.getChannel(name).getSessionId();
  }

  private IRDataLocation localData(String name) {
    return gen.env.getChannel(name).getLocalData();
  }

  private IRDataLocation remoteData(String name) {
    return gen.env.getChannel(name).getRemoteData();
  }

  private void addMove(String targetChannel, String fromChannel, IRSlotTree slots) {
    if (!slots.isLeaf()) {
      gen.block.add(new IRMoveValue(remoteData(targetChannel), localData(fromChannel), slots));
    }
  }

  private void addMove(String targetChannel, String fromChannel, IRSlot slot) {
    addMove(targetChannel, fromChannel, IRSlotTree.of(slot));
  }

  private boolean isPositive(ASTType type) {
    if (type instanceof ASTNotT) {
      return !isPositive(((ASTNotT) type).getin());
    }
    if (!(type instanceof ASTIdT)) {
      return gen.env.isPositive(type);
    }
    ASTIdT idType = (ASTIdT) type;

    if (recursionLabels.containsKey(idType.getid())) {
      return true;
    } else if (coRecursionLabels.containsKey(idType.getid())) {
      return false;
    }
    if (varTypes.containsKey(idType.getid())) {
      return gen.env.isPositive(varTypes.get(idType.getid()));
    } else {
      return gen.env.getType(idType.getid()).isPositive();
    }
  }

  private ASTType instType(ASTType polyType) {
    Env<EnvEntry> ep = gen.env.getEp();
    for (String var : varTypes.keySet()) {
      ep = ep.assoc(var, new TypeEntry(varTypes.get(var)));
    }
    try {
      return polyType.unfoldType(ep);
    } catch (Exception e) {
      throw new RuntimeException("Error unfolding type " + polyType, e);
    }
  }

  private boolean isValueInst(ASTType type, boolean expectedPolarity) {
    return IRValueChecker.check(
        gen.compiler, gen.env, instType(type), Optional.of(expectedPolarity));
  }

  private boolean isValuePoly(ASTType type, boolean expectedPolarity) {
    return IRValueChecker.check(gen.compiler, gen.env, type, Optional.of(expectedPolarity));
  }

  private static String newChannelName(String prefix) {
    return prefix + "_poly_" + (nextNameId++);
  }

  private IRSlotsFromASTType polySlotsFromType(ASTType type) {
    return gen.slotsFromType(type);
  }

  private IRSlotsFromASTType instSlotsFromType(ASTType type) {
    return gen.slotsFromType(instType(type));
  }

  private void advanceOrResetPoly(IRSlot slot, ASTType remainder, boolean isPositive) {
    gen.advanceOrReset(poly, slot, remainder, isPositive);
  }

  private void advanceOrResetInst(IRSlot slot, ASTType remainder, boolean isPositive) {
    gen.advanceOrReset(inst, slot, instType(remainder), isPositive);
  }

  private void advanceOrResetInst(
      IRSlotCombinations combinations, ASTType remainder, boolean isPositive) {
    gen.advanceOrReset(inst, combinations, instType(remainder), isPositive);
  }

  private void advanceOrResetBoth(IRSlot slot, ASTType remainder, boolean isPositive) {
    advanceOrResetPoly(slot, remainder, isPositive);
    advanceOrResetInst(slot, remainder, isPositive);
  }

  private void bothAdvanceOrReset(
      IRSlotCombinations combinations, ASTType remainder, boolean isPositive) {
    gen.advanceOrReset(poly, combinations, remainder, isPositive);
    gen.advanceOrReset(inst, combinations, instType(remainder), isPositive);
  }

  private boolean isPolymorphic(ASTType type) {
    Set<String> vars = new HashSet<>(varTypes.keySet());
    vars.addAll(recursionLabels.keySet());
    vars.addAll(coRecursionLabels.keySet());
    return IRUsesTypeVar.check(type, vars);
  }
}
