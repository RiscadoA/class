package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.Env;
import pt.inescid.cllsj.EnvEntry;
import pt.inescid.cllsj.TypeEntry;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.slot.*;

// From a given AST type, this visitor generates a IR slot corresponding to the root type,
// a IRSlotCombinations corresponding to all the local data requirements,
// and a IRSlotCombinations corresponding to all the remote data requirements.
public class IRSlotsFromASTType extends ASTTypeVisitor {
  private Compiler compiler;
  private Env<EnvEntry> ep;
  private IREnvironment env;
  private Set<String> recursionTypes;

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

  public static IRSlotsFromASTType compute(
      Compiler compiler,
      Env<EnvEntry> ep,
      IREnvironment env,
      Set<String> recursionTypes,
      ASTType type) {
    IRSlotsFromASTType visitor = new IRSlotsFromASTType();
    visitor.compiler = compiler;
    visitor.ep = ep;
    type.accept(visitor);
    return visitor;
  }

  private IRSlotsFromASTType recurse(ASTType type) {
    return compute(compiler, ep, env, recursionTypes, type);
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
    if (IRValueChecker.check(ep, env, type.getin(), Optional.of(true))) {
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
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTIdT type) {
    // Unfold the type to check if its definition is known
    ASTType unfolded;
    try {
      unfolded = type.unfoldType(ep);
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

    // If it is a recursion variable, then stop here
    if (recursionTypes.contains(type.getid())) {
      return;
    }

    // Otherwise, use the type slot
    IREnvironment.Type envType = env.getType(type.getid());
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
    ep = ep.assoc(type.getid(), new TypeEntry(new ASTIdT(type.getid())));
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTRecvT type) {
    // If the left-hand-side is a value, we start by visiting it
    // Otherwise, we just add a session slot
    if (compiler.optimizeSendValue.get()
        && IRValueChecker.check(ep, env, type.getlhs(), Optional.of(false))) {
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
        && IRValueChecker.check(ep, env, type.getlhs(), Optional.of(true))) {
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
    if (IRValueChecker.check(ep, env, type.getin(), Optional.of(false))) {
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
    remoteSlot(new IRTypeS());
  }

  @Override
  public void visit(ASTRecvTT type) {
    localSlot(new IRTypeS());
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
