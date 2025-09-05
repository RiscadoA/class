package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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

    gen.env = gen.env.addSession(poly, gen.slotsFromType(polyType, varTypes).combinations());

    IREnvironment.Channel instChannel = gen.env.getChannel(inst);
    IREnvironment.Channel polyChannel = gen.env.getChannel(poly);
    IRBlock contBlock = gen.process.createBlock("poly_cont");

    gen.block.add(
        new IRInitializeSession(
            polyChannel.getSessionId(), contBlock.getLocation(), polyChannel.getLocalData()));

    // We must copy any already written data from the instantiated channel to the
    // polymorphic channel

    // If polyType is negative, then the instantiated channel has already written
    // data to its remote,

    gen.addContinueIfNegative(polyChannel.getSessionId(), polyType);
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
      translator.varSlots.put(entry.getKey(), gen.slotsFromType(entry.getValue(), varTypes));
    }
    translator.poly = poly;
    translator.inst = inst;
    polyType.accept(translator);
  }

  private void recurse(ASTType polyType) {
    polyType.accept(this);
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
    addFinish(inst);
  }

  @Override
  public void visit(ASTBotT type) {
    addFinish(poly);
  }

  @Override
  public void visit(ASTintT type) {
    addLet(inst, poly, new IRIntS());
  }

  @Override
  public void visit(ASTCointT type) {
    addLet(poly, inst, new IRIntS());
  }

  @Override
  public void visit(ASTLintT type) {
    addLet(inst, poly, new IRIntS());
  }

  @Override
  public void visit(ASTLCointT type) {
    addLet(poly, inst, new IRIntS());
  }

  @Override
  public void visit(ASTLboolT type) {
    addLet(inst, poly, new IRBoolS());
  }

  @Override
  public void visit(ASTCoLboolT type) {
    addLet(poly, inst, new IRBoolS());
  }

  @Override
  public void visit(ASTLstringT type) {
    addLet(inst, poly, new IRStringS());
  }

  @Override
  public void visit(ASTCoLstringT type) {
    addLet(poly, inst, new IRStringS());
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
            gen.advanceOrReset(poly, new IRTagS(), caseType, true);
            gen.advanceOrReset(inst, new IRTagS(), caseType, true);

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
            gen.advanceOrReset(poly, new IRTagS(), caseType, false);
            gen.advanceOrReset(inst, new IRTagS(), caseType, false);

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
    // TODO: we can perform this optimization
    // // If the left-hand-side does not use the variable we're translating,
    // // we just send the value directly.
    // addMove(instChannel, polyChannel, new IRSessionS());
    // gen.advanceOrReset(polyChannel, new IRSessionS(), type.getrhs(), false);
    // gen.advanceOrReset(instChannel, new IRSessionS(), type.getrhs(), false);

    if (gen.compiler.optimizeSendValue.get() && isValue(type.getlhs(), true)) {
      throw new UnsupportedOperationException("Unimplemented");
    } else {
      // Otherwise, we'll need to store two news session
      String newPoly = newChannelName(poly);
      String newInst = newChannelName(inst);
      gen.env = gen.env.addSession(newPoly, slotsFromType(type.getlhs()).remoteCombinations());
      gen.env = gen.env.addSession(newInst, slotsFromType(type.getlhs()).localCombinations());

      // Receive the new session from the instantiated side
      gen.block.add(new IRBindSession(localData(inst), sessionId(newInst), localData(newInst)));
      gen.advanceOrReset(inst, new IRSessionS(), type.getrhs(), false);

      // Initialize the new polymorphic session with a new closure block.
      IRBlock closureBlock = gen.process.createBlock("poly_recv_closure");
      gen.block.add(
          new IRInitializeSession(
              sessionId(newPoly), closureBlock.getLocation(), localData(newPoly)));
      gen.recurse(
          closureBlock,
          () -> {
            gen.addContinueIfNegative(sessionId(newInst), type.getlhs());
            recurse(newPoly, newInst, type.getlhs());
          });

      // Write the new polymorphic session to the polymorphic side
      gen.block.add(new IRWriteSession(remoteData(poly), sessionId(newPoly)));
      gen.advanceOrReset(poly, new IRSessionS(), type.getrhs(), false);

      gen.addContinueIfPositive(sessionId(poly), type.getrhs());
    }

    recurse(type.getrhs());
  }

  @Override
  public void visit(ASTSendT type) {

    // TODO: possible error in id_pos/id_neg tests
    // a session is sent using send forward

    // recv x(a); // jump
    // send y(a);

    // recv y(a); // should not continue again

    // Pretty similar to the above, but dual

    if (gen.compiler.optimizeSendValue.get() && isValue(type.getlhs(), false)) {
      // If the left-hand-side is a value,
      throw new UnsupportedOperationException("Unimplemented");
    } else {
      // Otherwise, we'll need to store two news session
      String newPoly = newChannelName(poly);
      String newInst = newChannelName(inst);
      gen.env = gen.env.addSession(newPoly, slotsFromType(type.getlhs()).localCombinations());
      gen.env = gen.env.addSession(newInst, slotsFromType(type.getlhs()).remoteCombinations());

      // Receive the new session from the polymorphic side
      gen.block.add(new IRBindSession(localData(poly), sessionId(newPoly), localData(newPoly)));
      gen.advanceOrReset(poly, new IRSessionS(), type.getrhs(), true);

      // Initialize the new instantiated session with a new closure block.
      IRBlock closureBlock = gen.process.createBlock("poly_send_closure");
      gen.block.add(
          new IRInitializeSession(
              sessionId(newInst), closureBlock.getLocation(), localData(newInst)));
      gen.recurse(
          closureBlock,
          () -> {
            gen.addContinueIfPositive(sessionId(newPoly), type.getlhs());
            recurse(newPoly, newInst, type.getlhs());
          });

      // Write the new instantiated session to the instantiated side
      gen.block.add(new IRWriteSession(remoteData(inst), sessionId(newInst)));
      gen.advanceOrReset(inst, new IRSessionS(), type.getrhs(), true);

      gen.addContinueIfNegative(sessionId(inst), type.getrhs());
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
      addMoveAndFinish(target, source, varSlots.get(type.getid()).activeTree());
    } else if (recursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(recursionLabels.get(type.getid())));
    } else if (coRecursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(coRecursionLabels.get(type.getid())));
    } else {
      addLet(target, source, new IRVarS(gen.env.getType(type.getid()).getId()));
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
      addMoveAndFinish(target, source, varSlots.get(type.getid()).activeTree());
    } else if (recursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(recursionLabels.get(type.getid())));
    } else if (coRecursionLabels.containsKey(type.getid())) {
      gen.block.add(new IRJump(coRecursionLabels.get(type.getid())));
    } else {
      addLet(target, source, new IRVarS(gen.env.getType(type.getid()).getId()));
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

  private void addFinish(String targetChannel) {
    gen.block.add(new IRFinishSession(sessionId(targetChannel), true));
  }

  private void addMoveAndFinish(String targetChannel, String fromChannel, IRSlotTree slots) {
    addMove(targetChannel, fromChannel, slots);
    addFinish(targetChannel);
  }

  private void addLet(String targetChannel, String fromChannel, IRSlot slot) {
    addMoveAndFinish(targetChannel, fromChannel, IRSlotTree.of(slot));
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

  private boolean isValue(ASTType type, boolean expectedPolarity) {
    if (type instanceof ASTNotT) {
      return isValue(((ASTNotT) type).getin(), !expectedPolarity);
    }

    if (!(type instanceof ASTIdT)) {
      return IRValueChecker.check(gen.compiler, gen.env, type, Optional.of(expectedPolarity));
    }

    ASTIdT idType = (ASTIdT) type;
    if (recursionLabels.containsKey(idType.getid())
        || coRecursionLabels.containsKey(idType.getid())) {
      return false;
    } else if (varTypes.containsKey(idType.getid())) {
      return IRValueChecker.check(
          gen.compiler, gen.env, varTypes.get(idType.getid()), Optional.of(expectedPolarity));
    } else {
      return IRValueChecker.check(gen.compiler, gen.env, type, Optional.of(expectedPolarity));
    }
  }

  private static String newChannelName(String prefix) {
    return prefix + "_poly_" + (nextNameId++);
  }

  private IRSlotsFromASTType slotsFromType(ASTType type) {
    return gen.slotsFromType(type, varTypes);
  }
}
