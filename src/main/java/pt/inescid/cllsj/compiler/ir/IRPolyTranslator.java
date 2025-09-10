package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.ir.expression.literal.IRStringLiteral;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.*;

// Generates code for translating a polymorphic variable to its instantiated counterpart
// and vice-versa.
public class IRPolyTranslator extends ASTTypeVisitor {
  private IRGenerator gen;
  private Map<String, ASTType> varTypes;
  private Set<String> modifiedVarTypes;
  private Map<String, IRSlotsFromASTType> varSlots;

  private static int nextNameId = 0;

  private List<RecursionEntry> recursionCalls = new ArrayList<>();
  private String poly;
  private String inst;

  private static class RecursionEntry {
    public String typeId;
    public ASTType type;
    public BiConsumer<String, String> call;
  }

  // Returns the name of the new channel created for the polymorphic side
  public static IREnvironment.Channel translateLinear(
      IRGenerator gen,
      Map<String, ASTType> varTypes,
      Set<String> modifiedVarTypes,
      ASTType type,
      String inst,
      boolean addContinue) {
    IRPolyTranslator translator = new IRPolyTranslator();
    translator.gen = gen;
    translator.varTypes = varTypes;
    translator.modifiedVarTypes = modifiedVarTypes;
    translator.varSlots = new HashMap<>();
    for (Map.Entry<String, ASTType> entry : varTypes.entrySet()) {
      translator.varSlots.put(entry.getKey(), gen.slotsFromType(entry.getValue()));
    }

    String poly = newChannelName(inst);

    gen.env = gen.env.addSession(poly, gen.slotsFromType(type).combinations());

    IREnvironment.Channel instChannel = gen.env.getChannel(inst);
    IREnvironment.Channel polyChannel = gen.env.getChannel(poly);
    IRBlock contBlock = gen.process.createBlock("poly_cont");

    gen.block.add(
        new IRInitializeSession(
            polyChannel.getSessionId(), contBlock.getLocation(), polyChannel.getLocalData()));

    gen.recurse(
        contBlock,
        () -> {
          if (addContinue) {
            gen.addContinueIfNegative(instChannel.getSessionId(), type);
          }
          translator.recurse(poly, inst, type);
        });

    return polyChannel;
  }

  // Returns the name of the new channel created for the polymorphic side
  public static IREnvironment.Channel translateExponential(
      IRGenerator gen,
      Map<String, ASTType> varTypes,
      Set<String> modifiedVarTypes,
      ASTWhyT type,
      String inst,
      boolean addContinue) {
    IRPolyTranslator translator = new IRPolyTranslator();
    translator.gen = gen;
    translator.varTypes = varTypes;
    translator.modifiedVarTypes = modifiedVarTypes;
    translator.varSlots = new HashMap<>();
    for (Map.Entry<String, ASTType> entry : varTypes.entrySet()) {
      translator.varSlots.put(entry.getKey(), gen.slotsFromType(entry.getValue()));
    }

    String poly = newChannelName(inst);

    IRSlotsFromASTType info = gen.slotsFromType(type);

    gen.env = gen.env.addSession(poly, info.combinations());
    IRBlock contBlock = gen.process.createBlock("poly_cont");
    gen.block.add(
        new IRInitializeSession(
            translator.sessionId(poly), contBlock.getLocation(), translator.localData(poly)));

    gen.env =
        gen.env.makeChannelExponential(
            poly,
            info.activeLocalTree,
            false,
            dropId -> {
              gen.block.add(new IRDeferDrop(dropId));
            });

    gen.addBang(
        poly,
        poly,
        translator.polyType(type.getin()),
        Set.of(inst),
        env -> IRPolyEndPointCounter.count(gen.compiler, env.withKnownTypes(varTypes), type.getin(), modifiedVarTypes),
        () -> {
          gen.env = gen.env.withKnownTypes(varTypes);
          gen.addCall(
              inst,
              inst,
              translator.instType(type.getin()),
              () -> {
                translator.recurse(poly, inst, type.getin());
              });
        });

    gen.block = contBlock;

    return gen.env.getChannel(poly);
  }

  private void recurse(ASTType type) {
    if (isPolymorphic(type)) {
      type.accept(this);
    } else {
      gen.addForward(poly, inst, type);
    }
  }

  private void recurse(String polyChannel, String instChannel, ASTType type) {
    String oldPolyChannel = this.poly;
    String oldInstChannel = this.inst;
    this.poly = polyChannel;
    this.inst = instChannel;
    recurse(type);
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
    final String inst = this.inst;
    final String poly = this.poly;

    gen.addWhy(
        poly,
        polyType(type.getin()),
        () -> {
          final String linInst = newChannelName(inst);
          gen.addBang(
              inst,
              linInst,
              instType(type.getin()),
              Set.of(poly),
              env ->
                  IRPolyEndPointCounter.count(
                      gen.compiler, env.withKnownTypes(varTypes), type.getin(), modifiedVarTypes),
              () -> {
                final String linPoly = newChannelName(poly);
                gen.env = gen.env.withKnownTypes(varTypes);
                gen.addCall(
                    poly,
                    linPoly,
                    polyType(type.getin()),
                    () -> {
                      recurse(linPoly, linInst, type.getin());
                    });
              });
        });
  }

  @Override
  public void visit(ASTWhyT type) {
    final String inst = this.inst;
    final String poly = this.poly;

    gen.addWhy(
        inst,
        instType(type.getin()),
        () -> {
          final String linPoly = newChannelName(poly);
          gen.addBang(
              poly,
              linPoly,
              polyType(type.getin()),
              Set.of(inst),
              env ->
                  IRPolyEndPointCounter.count(
                      gen.compiler, env.withKnownTypes(varTypes), type.getin(), modifiedVarTypes),
              () -> {
                final String linInst = newChannelName(inst);
                gen.env = gen.env.withKnownTypes(varTypes);
                gen.addCall(
                    inst,
                    linInst,
                    instType(type.getin()),
                    () -> {
                      recurse(linPoly, linInst, type.getin());
                    });
              });
        });
  }

  @Override
  public void visit(ASTOfferT type) {
    final String poly = this.poly;
    final String inst = this.inst;

    // Here the instantiated side sends a tag to be received by the polymorphic side

    addMove(poly, inst, new IRTagS());

    List<IRBranch.Case> cases = new ArrayList<>();

    for (int i = 0; i < type.getcases().size(); ++i) {
      String label = type.getLabel(i);
      ASTType caseType = type.getCaseType(label);

      IRBlock caseBlock = gen.process.createBlock("poly_offer_" + label.substring(1).toLowerCase());
      cases.add(
          new IRBranch.Case(
              caseBlock.getLocation(),
              IRPolyEndPointCounter.count(gen.compiler, gen.env, caseType, modifiedVarTypes)));
      gen.recurse(
          caseBlock,
          () -> {
            advanceOrResetBoth(new IRTagS(), caseType, false);

            // The poly channel was negative, and the instantiated channel was positive
            // If the poly type becomes positive, we must jump to it
            gen.addContinueIfPositive(sessionId(poly), caseType);

            recurse(caseType);
          });
    }

    gen.block.add(new IRBranchTag(localData(inst), cases));
  }

  @Override
  public void visit(ASTCaseT type) {
    final String poly = this.poly;
    final String inst = this.inst;

    // Here the polymorphic side sends a tag to be received by the instantiated side

    addMove(inst, poly, new IRTagS());

    List<IRBranch.Case> cases = new ArrayList<>();

    for (int i = 0; i < type.getcases().size(); ++i) {
      String label = type.getLabel(i);
      ASTType caseType = type.getCaseType(label);

      IRBlock caseBlock = gen.process.createBlock("poly_case_" + label.substring(1).toLowerCase());
      cases.add(
          new IRBranch.Case(
              caseBlock.getLocation(),
              IRPolyEndPointCounter.count(gen.compiler, gen.env, caseType, modifiedVarTypes)));
      gen.recurse(
          caseBlock,
          () -> {
            advanceOrResetBoth(new IRTagS(), caseType, true);

            // The poly channel was positive, and the instantiated channel was negative
            // If the instantiated channel becomes positive, we must jump to it
            gen.addContinueIfNegative(sessionId(inst), caseType);

            recurse(caseType);
          });
    }

    gen.block.add(new IRBranchTag(localData(poly), cases));
  }

  private void visitRec(String typeId, ASTType inner, boolean isRec) {
    final ASTType rec = isRec ? new ASTRecT(typeId, inner) : new ASTCoRecT(typeId, inner);
    final String poly = this.poly;
    final String inst = this.inst;

    for (RecursionEntry entry : recursionCalls) {
      if (entry.type.equals(rec)) {
        entry.call.accept(poly, inst);
        return;
      }
    }

    Map<Boolean, List<IRCallProcess.TypeArgument>> typeArguments = new HashMap<>();

    // Function used to create the recursive and co-recursive processes
    BiFunction<Boolean, IRProcessId, IREnvironment> createProcess = (innerIsRec, processId) -> {
      // Figure out the types
      ASTType innerType = (isRec == innerIsRec) ? inner : dual(inner);

      // Create a new empty process and environment
      IRProcess process = new IRProcess(processId);
      IREnvironment newEnv = new IREnvironment(process, gen.env.getEp());

      // Prepare lists of arguments to pass to the processes
      // Pass all types in the current environment to the polymorphic process
      typeArguments.put(innerIsRec, new ArrayList<>());
      for (int i = 0; i < gen.process.getTypeCount(); ++i) {
        IRTypeId id = new IRTypeId(i);
        IREnvironment.Type envType = gen.env.getType(id);
        newEnv = newEnv.addType(envType.getName(), envType.isPositive());
        IRTypeId newId = newEnv.getType(envType.getName()).getId();
        typeArguments.get(innerIsRec).add(new IRCallProcess.TypeArgument(IRSlotTree.of(new IRVarS(id)), newId));
      }
      newEnv = newEnv.withKnownTypes(varTypes);

      // Count the end points of the process
      process.setEndPoints(IRPolyEndPointCounter.count(gen.compiler, newEnv, innerType, modifiedVarTypes));

      // Define the argument sessions for the process
      IRSlotsFromASTType polyInfo = IRSlotsFromASTType.compute(gen.compiler, newEnv, polyType(innerType));
      IRSlotsFromASTType instInfo = IRSlotsFromASTType.compute(gen.compiler, newEnv, instType(innerType));
      newEnv = newEnv.addArgSession(poly, polyInfo.combinations());
      newEnv = newEnv.addArgSession(inst, instInfo.combinations());

      return newEnv;
    };

    final IRProcessId recProcessId = gen.genChildProcessId("poly_rec");
    final IRProcessId corecProcessId = gen.genChildProcessId("poly_corec");
    final IREnvironment recEnv = createProcess.apply(false, recProcessId);
    final IREnvironment corecEnv = createProcess.apply(true, corecProcessId);

    Function<Boolean, BiConsumer<String, String>> call =
        innerIsRec -> (innerPoly, innerInst) -> {
          final ASTType recType = (isRec == innerIsRec) ? rec : dual(rec);
          final ASTType innerType = (isRec == innerIsRec) ? inner : dual(inner);

          // Unfold, must jump to both channels
          gen.addContinue(sessionId(innerPoly));
          gen.addContinue(sessionId(innerInst));

          if (isPositive(polyType(recType))) {
            gen.addContinueIfNegative(sessionId(innerPoly), polyType(innerType));
          }
          if (isPositive(instType(recType))) {
            gen.addContinueIfNegative(sessionId(innerInst), instType(innerType));
          }

          gen.env = gen.env.resetChannel(innerPoly);
          gen.env = gen.env.resetChannel(innerInst);

          IREnvironment procEnv = innerIsRec ? recEnv : corecEnv;

          IRSessionId targetPolySession = procEnv.getChannel(poly).getSessionId();
          IRSessionId targetInstSession = procEnv.getChannel(inst).getSessionId();
          IRLocalDataId targetPolyData = procEnv.getChannel(poly).getLocalDataId();
          IRLocalDataId targetInstData = procEnv.getChannel(inst).getLocalDataId();

          IRSlotsFromASTType polyInfo = IRSlotsFromASTType.compute(gen.compiler, procEnv, polyType(innerType));
          IRSlotsFromASTType instInfo = IRSlotsFromASTType.compute(gen.compiler, procEnv, instType(innerType));

          gen.procUsed.add(innerIsRec ? recProcessId : corecProcessId);
          gen.block.add(
              new IRCallProcess(
                  innerIsRec ? recProcessId : corecProcessId,
                  typeArguments.get(innerIsRec),
                  List.of(
                    new IRCallProcess.SessionArgument(
                          sessionId(innerPoly), targetPolySession, IRSlotOffset.ZERO),
                      new IRCallProcess.SessionArgument(
                          sessionId(innerInst), targetInstSession, IRSlotOffset.ZERO)
                  ),
                  List.of(
                    new IRCallProcess.DataArgument(
                        localData(innerPoly), targetPolyData, polyInfo.activeLocalTree, false),
                        new IRCallProcess.DataArgument(
                        localData(innerInst), targetInstData, instInfo.activeLocalTree, false)),
                  true));
        };

    // Store the processes we just defined so that we can generate it later
    for (boolean innerIsRec : new boolean[]{false, true}) {
      final IRProcessId processId = innerIsRec ? recProcessId : corecProcessId;
      final IREnvironment env = innerIsRec ? recEnv : corecEnv;
      final ASTType recType = (innerIsRec == isRec) ? rec : dual(rec);
      final ASTType innerType = (innerIsRec == isRec) ? inner : dual(inner);

      gen.procGens.put(
        processId,
        () -> {
          RecursionEntry entry = new RecursionEntry();
          entry.typeId = typeId;
          entry.type = recType;
          entry.call = call.apply(innerIsRec);

          RecursionEntry dualEntry = new RecursionEntry();
          dualEntry.typeId = typeId;
          dualEntry.type = dual(recType);
          dualEntry.call = call.apply(!innerIsRec);

          recursionCalls.addLast(entry);
          recursionCalls.addLast(dualEntry);
          recurse(poly, inst, innerType);
          recursionCalls.removeLast();
          recursionCalls.removeLast();
        });

      gen.procEnvs.put(processId, env);
    };

    call.apply(isRec).accept(poly, inst);
  }

  @Override
  public void visit(ASTRecT type) {
    visitRec(type.getid(), type.getin(), true);
  }

  @Override
  public void visit(ASTCoRecT type) {
    visitRec(type.getid(), type.getin(), false);
  }

  @Override
  public void visit(ASTRecvT type) {
    String recvInst = newChannelName(inst);
    String sentPoly = newChannelName(poly);
    gen.addRecv(
        inst,
        recvInst,
        instType(type.getlhs()),
        instType(type.getrhs()),
        () -> {
          gen.addSend(
              poly,
              sentPoly,
              polyType(type.getlhs()),
              polyType(type.getrhs()),
              () -> {
                recurse(sentPoly, recvInst, type.getlhs());
              },
              () -> {
                recurse(poly, inst, type.getrhs());
              });
        });
  }

  @Override
  public void visit(ASTSendT type) {
    String recvPoly = newChannelName(poly);
    String sentInst = newChannelName(inst);
    gen.addRecv(
        poly,
        recvPoly,
        polyType(type.getlhs()),
        polyType(type.getrhs()),
        () -> {
          gen.addSend(
              inst,
              sentInst,
              instType(type.getlhs()),
              instType(type.getrhs()),
              () -> {
                recurse(recvPoly, sentInst, type.getlhs());
              },
              () -> {
                recurse(poly, inst, type.getrhs());
              });
        });
  }

  @Override
  public void visit(ASTSendTT type) {
    final String poly = this.poly;
    final String inst = this.inst;

    String recvTyid = ASTType.gensym();
    ASTType rhsType =
        type.getrhs().subst(new Env<ASTType>().assoc(type.getid(), new ASTIdT(recvTyid)));

    gen.addRecvTy(
        poly,
        recvTyid,
        Set.of(type.getid()),
        Map.of(inst, instType(type)),
        polyType(rhsType),
        env -> (1
            + IRPolyEndPointCounter.count(gen.compiler, env.withKnownTypes(varTypes), type.getrhs(), Set.of())
            + IRPolyEndPointCounter.count(gen.compiler, env.withKnownTypes(varTypes), rhsType, modifiedVarTypes)),
        () -> {
          gen.env = gen.env.withKnownTypes(varTypes);
          gen.addSendTy(
              inst,
              type.getid(),
              new ASTIdT(recvTyid),
              instType(rhsType),
              instType(type.getrhs()),
              () -> {
                recurse(poly, inst, rhsType);
              });
        });
  }

  @Override
  public void visit(ASTRecvTT type) {
    final String poly = this.poly;
    final String inst = this.inst;

    String recvTyid = ASTType.gensym();
    ASTType rhsType =
        type.getrhs().subst(new Env<ASTType>().assoc(type.getid(), new ASTIdT(recvTyid)));

    gen.addRecvTy(
        inst,
        recvTyid,
        Set.of(type.getid()),
        Map.of(poly, polyType(type)),
        instType(rhsType),
        env -> (1 + IRPolyEndPointCounter.count(gen.compiler, env.withKnownTypes(varTypes), dual(type.getrhs()), Set.of()) + IRPolyEndPointCounter.count(gen.compiler, env, rhsType, modifiedVarTypes)),
        () -> {
          gen.env = gen.env.withKnownTypes(varTypes);
          gen.addSendTy(
              poly,
              type.getid(),
              new ASTIdT(recvTyid),
              polyType(rhsType),
              polyType(type.getrhs()),
              () -> {
                recurse(poly, inst, rhsType);
              });
        });
  }

  @Override
  public void visit(ASTIdT idType) {
    ASTType type = idType.unfoldTypeCatch(gen.env.getEp());
    if (!(type instanceof ASTIdT)) {
      type.accept(this);
      return;
    }
    idType = (ASTIdT) type;

    if (varTypes.containsKey(idType.getid())) {
      gen.addForward(poly, inst, instType(idType));
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
    ASTIdT idType = (ASTIdT) notType.getin();
    ASTType type = idType.unfoldTypeCatch(gen.env.getEp());
    if (!(type instanceof ASTIdT)) {
      type.dualCatch(gen.env.getEp()).accept(this);
      return;
    }
    idType = (ASTIdT) type;

    if (varTypes.containsKey(idType.getid())) {
      gen.addForward(poly, inst, instType(dual(idType)));
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
    try {
      type = type.unfoldType(gen.env.getEp());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    if (!(type instanceof ASTIdT)) {
      return gen.env.isPositive(type);
    }
    ASTIdT idType = (ASTIdT) type;

    if (varTypes.containsKey(idType.getid())) {
      return gen.env.isPositive(varTypes.get(idType.getid()));
    } else {
      return gen.env.getType(idType.getid()).isPositive();
    }
  }

  private ASTType polyType(ASTType type) {
    return dual(type.unfoldTypeCatch(gen.env.getEp()));
  }

  private ASTType instType(ASTType type) {
    for (String var : varTypes.keySet()) {
      Env<ASTType> ee = new Env<ASTType>().assoc(var, varTypes.get(var));
      type = type.unfoldTypeCatch(gen.env.getEp()).subst(ee);
    }
    return type;
  }

  private static String newChannelName(String prefix) {
    return prefix + "_poly_" + (nextNameId++);
  }

  private void advanceOrResetPoly(IRSlot slot, ASTType remainder, boolean isPositive) {
    gen.advanceOrReset(poly, slot, remainder, isPositive);
  }

  private void advanceOrResetInst(IRSlot slot, ASTType remainder, boolean isPositive) {
    gen.advanceOrReset(inst, slot, instType(remainder), isPositive);
  }

  private void advanceOrResetBoth(IRSlot slot, ASTType remainder, boolean isPositive) {
    advanceOrResetPoly(slot, remainder, isPositive);
    advanceOrResetInst(slot, remainder, isPositive);
  }

  private boolean isPolymorphic(ASTType type) {
    Set<String> vars = new HashSet<>(modifiedVarTypes);
    vars.addAll(recursionCalls.stream().map(e -> e.typeId).toList());
    return IRUsesTypeVar.check(gen.env.getEp(), type, vars);
  }

  private ASTType dual(ASTType type) {
    return type.dualCatch(gen.env.getEp());
  }
}
