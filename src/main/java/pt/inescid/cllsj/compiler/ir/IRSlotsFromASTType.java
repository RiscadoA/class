package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.slot.*;

// From a given AST type, this visitor generates a IR slot corresponding to the root type,
// a IRSlotCombinations corresponding to all the local data requirements,
// and a IRSlotCombinations corresponding to all the remote data requirements.
public class IRSlotsFromASTType extends ASTTypeVisitor {
  private Compiler compiler;
  private IREnvironment env;
  private Set<String> recursiveIds;

  // Slot corresponding to the root type
  public Optional<IRSlot> slot = Optional.empty();

  // Possible slot combinations for the current negative segment of the type
  // I.e., what data is currently being read from the local side
  // This must be a IRSlotsCombinations as we may have choice types
  public IRSlotTree activeLocalTree = IRSlotTree.LEAF;

  // Similar to above, but for the remote side (positive segment)
  // I.e., what data is currently being written to the remote side
  public IRSlotTree activeRemoteTree = IRSlotTree.LEAF;

  // Possible slot combinations for all remaining negative segments of the type,
  // excluding the current one.
  // I.e., what data will be read from the local side in the future
  public IRSlotCombinations remainderLocalCombinations = IRSlotCombinations.EMPTY;

  // Similar to above, but for the remote side (positive segments)
  // I.e., what data will be written to the remote side in the future
  public IRSlotCombinations remainderRemoteCombinations = IRSlotCombinations.EMPTY;

  // Returns all local slot combinations, both active and remainder
  public IRSlotCombinations localCombinations() {
    return activeLocalTree.combinations().merge(remainderLocalCombinations);
  }

  // Returns all remote slot combinations, both active and remainder
  public IRSlotCombinations remoteCombinations() {
    return activeRemoteTree.combinations().merge(remainderRemoteCombinations);
  }

  // Returns all slot combinations, both local and remote, active and remainder
  public IRSlotCombinations combinations() {
    return localCombinations().merge(remoteCombinations());
  }

  // Checks if the type has any local data requirements
  public boolean hasLocalData() {
    return !localCombinations().isEmpty();
  }

  // Checks if the type has any remote data requirements
  public boolean hasRemoteData() {
    return !remoteCombinations().isEmpty();
  }

  // Gets the active slot tree
  public IRSlotTree activeTree() {
    if (!activeLocalTree.isLeaf()) {
      return activeLocalTree;
    } else {
      return activeRemoteTree;
    }
  }
  
  public static IRSlotsFromASTType compute(Compiler compiler, IREnvironment env, ASTType type) {
    return compute(compiler, env, new HashSet<>(), type);
  }

  private static IRSlotsFromASTType compute(Compiler compiler, IREnvironment env, Set<String> recursiveIds, ASTType type) {
    IRSlotsFromASTType visitor = new IRSlotsFromASTType();
    visitor.compiler = compiler;
    visitor.env = env;
    visitor.recursiveIds = recursiveIds;
    type.accept(visitor);
    return visitor;
  }

  private IRSlotsFromASTType recurse(ASTType type, IREnvironment env) {
    return compute(compiler, env, new HashSet<>(recursiveIds), type);
  }

  private IRSlotsFromASTType recurse(ASTType type) {
    return recurse(type, env);
  }

  private void localSlot(IRSlot slot) {
    this.slot = Optional.of(slot);
    activeLocalTree = IRSlotTree.of(slot);
  }

  private void remoteSlot(IRSlot slot) {
    this.slot = Optional.of(slot);
    activeRemoteTree = IRSlotTree.of(slot);
  }

  @Override
  public void visit(ASTBangT type) {
    if (IRValueChecker.check(compiler, env, type.getin(), Optional.of(true))) {
      type.getin().accept(this);
    } else {
      remoteSlot(new IRExponentialS());
    }
  }

  @Override
  public void visit(ASTBotT type) {}

  @Override
  public void visit(ASTCaseT type) {
    slot = Optional.of(new IRTagS());

    List<IRSlotTree> cases = new ArrayList<>();
    for (ASTType choice : type.getcases().values()) {
      IRSlotsFromASTType result = recurse(choice);

      cases.add(result.activeRemoteTree);
      remainderLocalCombinations = remainderLocalCombinations.merge(result.localCombinations());
      remainderRemoteCombinations =
          remainderRemoteCombinations.merge(result.remainderRemoteCombinations);
    }
    activeRemoteTree = new IRSlotTree.Tag(cases);
  }

  @Override
  public void visit(ASTCoRecT type) {
    if (recursiveIds.add(type.getid())) {
      IRSlotsFromASTType result = recurse(type.getin(), env);
      slot = Optional.empty();
      remainderLocalCombinations = result.localCombinations();
      remainderRemoteCombinations = result.remoteCombinations();
    }
  }

  @Override
  public void visit(ASTIdT type) {
    // Unfold the type to check if its definition is known
    ASTType unfolded;
    try {
      unfolded = type.unfoldType(env.getEp());
    } catch (Exception e) {
      throw new IllegalArgumentException("Error unfolding type: " + e.getMessage());
    }
    if (!(unfolded instanceof ASTIdT)) {
      // We have a type definition, just recurse on the unfolded type
      unfolded.accept(this);
      return;
    }

    // We still have a type identifier
    type = (ASTIdT) unfolded;

    // Otherwise, use the environment to find more about the type
    IREnvironment.Type envType = env.getType(type.getid());

    if (envType.isRecursive()) {
      return;
    }

    // If it is a known type, use the known slot combinations
    if (envType.isKnown()) {
      IRSlotsFromASTType result = recurse(envType.getKnown());
      if (envType.isPositive()) {
        slot = Optional.of(new IRKnownVarS(result.activeRemoteTree));
        activeRemoteTree = IRSlotTree.of(slot.get());
      } else {
        slot = Optional.of(new IRKnownVarS(result.activeLocalTree));
        activeLocalTree = IRSlotTree.of(slot.get());
      }
      return;
    }

    // Otherwise, just create a variable slot
    slot = Optional.of(new IRVarS(envType.getId()));
    if (envType.isPositive()) {
      activeRemoteTree = IRSlotTree.of(slot.get());
    } else {
      activeLocalTree = IRSlotTree.of(slot.get());
    }
  }

  @Override
  public void visit(ASTNotT type) {
    type.getin().accept(this);

    IRSlotTree swapActive = activeLocalTree;
    IRSlotCombinations swapRemainder = remainderLocalCombinations;
    activeLocalTree = activeRemoteTree;
    remainderLocalCombinations = remainderRemoteCombinations;
    activeRemoteTree = swapActive;
    remainderRemoteCombinations = swapRemainder;
  }

  @Override
  public void visit(ASTOfferT type) {
    slot = Optional.of(new IRTagS());

    List<IRSlotTree> cases = new ArrayList<>();
    for (ASTType choice : type.getcases().values()) {
      IRSlotsFromASTType result = recurse(choice);

      cases.add(result.activeLocalTree);
      remainderLocalCombinations =
          remainderLocalCombinations.merge(result.remainderLocalCombinations);
      remainderRemoteCombinations = remainderRemoteCombinations.merge(result.remoteCombinations());
    }
    activeLocalTree = new IRSlotTree.Tag(cases);
  }

  @Override
  public void visit(ASTOneT type) {}

  @Override
  public void visit(ASTRecT type) {
    if (recursiveIds.add(type.getid())) {
      IRSlotsFromASTType result = recurse(type.getin(), env);
      slot = Optional.empty();
      remainderLocalCombinations = result.localCombinations();
      remainderRemoteCombinations = result.remoteCombinations();
    }
  }

  @Override
  public void visit(ASTRecvT type) {
    // If the left-hand-side is a value, we start by visiting it
    // Otherwise, we just add a session slot
    if (compiler.optimizeSendValue.get()
        && IRValueChecker.check(compiler, env, type.getlhs(), Optional.of(false))) {
      type.getlhs().accept(this);
    } else {
      localSlot(new IRSessionS());
    }

    // Merge with the right-hand-side
    IRSlotsFromASTType rhsResult = recurse(type.getrhs());
    activeLocalTree = activeLocalTree.suffix(rhsResult.activeLocalTree);
    remainderLocalCombinations = rhsResult.remainderLocalCombinations;
    remainderRemoteCombinations = rhsResult.remoteCombinations();
  }

  @Override
  public void visit(ASTSendT type) {
    // If the left-hand-side can be a value, we start by visiting it
    // Otherwise, we just add a session slot
    if (compiler.optimizeSendValue.get()
        && IRValueChecker.check(compiler, env, type.getlhs(), Optional.of(true))) {
      type.getlhs().accept(this);
    } else {
      remoteSlot(new IRSessionS());
    }

    // Merge with the right-hand-side
    IRSlotsFromASTType rhsResult = recurse(type.getrhs());
    activeRemoteTree = activeRemoteTree.suffix(rhsResult.activeRemoteTree);
    remainderLocalCombinations = rhsResult.localCombinations();
    remainderRemoteCombinations = rhsResult.remainderRemoteCombinations;
  }

  @Override
  public void visit(ASTWhyT type) {
    if (IRValueChecker.check(compiler, env, type.getin(), Optional.of(false))) {
      type.getin().accept(this);
    } else {
      localSlot(new IRExponentialS());
    }
  }

  @Override
  public void visit(ASTintT type) {
    remoteSlot(new IRIntS());
  }

  @Override
  public void visit(ASTCointT type) {
    localSlot(new IRIntS());
  }

  @Override
  public void visit(ASTLintT type) {
    remoteSlot(new IRIntS());
  }

  @Override
  public void visit(ASTLCointT type) {
    localSlot(new IRIntS());
  }

  @Override
  public void visit(ASTLboolT type) {
    remoteSlot(new IRBoolS());
  }

  @Override
  public void visit(ASTCoLboolT type) {
    localSlot(new IRBoolS());
  }

  @Override
  public void visit(ASTLstringT type) {
    remoteSlot(new IRStringS());
  }

  @Override
  public void visit(ASTCoLstringT type) {
    localSlot(new IRStringS());
  }

  @Override
  public void visit(ASTSendTT type) {
    this.slot = Optional.of(new IRTypeS());
    activeRemoteTree =
        IRSlotTree.of(IRSlotSequence.of(new IRTypeS(), new IRSessionS(), new IRTagS()));
  }

  @Override
  public void visit(ASTRecvTT type) {
    this.slot = Optional.of(new IRTypeS());
    activeLocalTree =
        IRSlotTree.of(IRSlotSequence.of(new IRTypeS(), new IRSessionS(), new IRTagS()));
  }

  @Override
  public void visit(ASTAffineT type) {
    throw new UnsupportedOperationException("Affine types should no longer exist at this stage");
  }

  @Override
  public void visit(ASTCoAffineT type) {
    throw new UnsupportedOperationException("Affine types should no longer exist at this stage");
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
}
