package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;
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
  private List<ASTType> visitedRecursive;

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
    return remainderLocalCombinations.merge(IRSlotCombinations.of(activeLocalTree));
  }

  // Returns all remote slot combinations, both active and remainder
  public IRSlotCombinations remoteCombinations() {
    return remainderRemoteCombinations.merge(IRSlotCombinations.of(activeRemoteTree));
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
    return compute(compiler, env, new ArrayList<>(), type);
  }

  private static IRSlotsFromASTType compute(
      Compiler compiler, IREnvironment env, List<ASTType> visitedRecursive, ASTType type) {
    IRSlotsFromASTType visitor = new IRSlotsFromASTType();
    visitor.compiler = compiler;
    visitor.env = env;
    visitor.visitedRecursive = visitedRecursive;
    type.accept(visitor);
    return visitor;
  }

  private IRSlotsFromASTType recurse(ASTType type, IREnvironment env) {
    return compute(compiler, env, visitedRecursive, type);
  }

  private IRSlotsFromASTType recurse(ASTType type) {
    return recurse(type, env);
  }

  private void localSlot(IRSlot slot) {
    activeLocalTree = IRSlotTree.of(slot);
  }

  private void remoteSlot(IRSlot slot) {
    activeRemoteTree = IRSlotTree.of(slot);
  }

  @Override
  public void visit(ASTBangT type) {
    IRTypeFlagRequisites reqs = IRIsCloneableChecker.check(compiler, env, type.getin(), true);
    IRSlotsFromASTType inner = recurse(type.getin());
    activeRemoteTree =
        IRSlotTree.type(reqs, inner.activeRemoteTree, IRSlotTree.of(new IRExponentialS()));
  }

  @Override
  public void visit(ASTBotT type) {}

  @Override
  public void visit(ASTCaseT type) {
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
    if (!visitedRecursive.contains(type)) {
      visitedRecursive.addLast(type);

      IRSlotsFromASTType result = recurse(type.getin(), env);
      remainderLocalCombinations = result.localCombinations();
      remainderRemoteCombinations = result.remoteCombinations();

      visitedRecursive.removeLast();
    }
  }

  @Override
  public void visit(ASTIdT type) {
    // Unfold the type to check if its definition is known
    ASTType unfolded = type.unfoldTypeCatch(env.getEp());
    if (!(unfolded instanceof ASTIdT)) {
      // We have a type definition, just recurse on the unfolded type
      unfolded.accept(this);
      return;
    }
    type = (ASTIdT) unfolded;

    // Otherwise, use the environment to create a variable slot
    IREnvironment.Type envType = env.getType(type.getid());
    if (envType.isPositive()) {
      remoteSlot(new IRVarS(envType.getId()));
    } else {
      localSlot(new IRVarS(envType.getId()));
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
    if (!visitedRecursive.contains(type)) {
      visitedRecursive.addLast(type);

      IRSlotsFromASTType result = recurse(type.getin(), env);
      remainderLocalCombinations = result.localCombinations();
      remainderRemoteCombinations = result.remoteCombinations();

      visitedRecursive.removeLast();
    }
  }

  @Override
  public void visit(ASTRecvT type) {
    IRTypeFlagRequisites reqs = IRIsValueChecker.check(compiler, env, type.getlhs(), false);
    if (!compiler.optimizeSendValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }

    IRSlotsFromASTType lhs = recurse(type.getlhs());
    activeLocalTree = IRSlotTree.type(reqs, lhs.activeLocalTree, IRSlotTree.of(new IRSessionS()));

    // Merge with the right-hand-side
    IRSlotsFromASTType rhsResult = recurse(type.getrhs());
    activeLocalTree = activeLocalTree.suffix(rhsResult.activeLocalTree);
    remainderLocalCombinations = rhsResult.remainderLocalCombinations;
    remainderRemoteCombinations = rhsResult.remoteCombinations();
  }

  @Override
  public void visit(ASTSendT type) {
    IRTypeFlagRequisites reqs = IRIsValueChecker.check(compiler, env, type.getlhs(), true);
    if (!compiler.optimizeSendValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }

    IRSlotsFromASTType lhs = recurse(type.getlhs());
    activeRemoteTree = IRSlotTree.type(reqs, lhs.activeRemoteTree, IRSlotTree.of(new IRSessionS()));

    // Merge with the right-hand-side
    IRSlotsFromASTType rhsResult = recurse(type.getrhs());
    activeRemoteTree = activeRemoteTree.suffix(rhsResult.activeRemoteTree);
    remainderLocalCombinations = rhsResult.localCombinations();
    remainderRemoteCombinations = rhsResult.remainderRemoteCombinations;
  }

  @Override
  public void visit(ASTWhyT type) {
    IRTypeFlagRequisites reqs = IRIsCloneableChecker.check(compiler, env, type.getin(), false);
    IRSlotsFromASTType inner = recurse(type.getin());
    activeLocalTree =
        IRSlotTree.type(reqs, inner.activeLocalTree, IRSlotTree.of(new IRExponentialS()));
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
    activeRemoteTree = IRSlotTree.of(new IRTypeS(), new IRSessionS(), new IRTagS());
  }

  @Override
  public void visit(ASTRecvTT type) {
    activeLocalTree = IRSlotTree.of(new IRTypeS(), new IRSessionS(), new IRTagS());
  }

  @Override
  public void visit(ASTAffineT type) {
    IRTypeFlagRequisites reqs = IRIsDroppableChecker.check(compiler, env, type.getin(), true);
    if (!compiler.optimizeAffineValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }

    IRSlotsFromASTType inner = recurse(type.getin());
    activeRemoteTree = IRSlotTree.type(reqs, inner.activeRemoteTree, IRSlotTree.LEAF);
    remainderLocalCombinations =
        inner
            .localCombinations()
            .merge(
                IRSlotTree.type(
                    reqs,
                    IRSlotTree.LEAF,
                    new IRSlotTree.Tag(List.of(IRSlotTree.LEAF, inner.activeLocalTree))));
    remainderRemoteCombinations = inner.remoteCombinations();
  }

  @Override
  public void visit(ASTCoAffineT type) {
    IRTypeFlagRequisites reqs = IRIsDroppableChecker.check(compiler, env, type.getin(), false);
    if (!compiler.optimizeAffineValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }

    IRSlotsFromASTType inner = recurse(type.getin());
    activeLocalTree = IRSlotTree.type(reqs, inner.activeLocalTree, IRSlotTree.LEAF);
    remainderLocalCombinations = inner.localCombinations();
    remainderRemoteCombinations =
        inner
            .remoteCombinations()
            .merge(
                IRSlotTree.type(
                    reqs,
                    IRSlotTree.LEAF,
                    new IRSlotTree.Tag(List.of(IRSlotTree.LEAF, inner.activeRemoteTree))));
  }

  @Override
  public void visit(ASTCellT type) {
    IRTypeFlagRequisites reqs = IRIsDroppableChecker.check(compiler, env, type.getin(), true);
    if (!compiler.optimizeAffineValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }
    IRSlotsFromASTType inner = recurse(type.getin());
    remoteSlot(
        new IRCellS(
            IRSlotTree.type(reqs, inner.activeRemoteTree, IRSlotTree.of(new IRSessionS()))));
  }

  @Override
  public void visit(ASTUsageT type) {
    IRTypeFlagRequisites reqs = IRIsDroppableChecker.check(compiler, env, type.getin(), false);
    if (!compiler.optimizeAffineValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }
    IRSlotsFromASTType inner = recurse(type.getin());
    localSlot(
        new IRCellS(IRSlotTree.type(reqs, inner.activeLocalTree, IRSlotTree.of(new IRSessionS()))));
  }

  @Override
  public void visit(ASTCellLT type) {
    IRTypeFlagRequisites reqs = IRIsDroppableChecker.check(compiler, env, type.getin(), true);
    if (compiler.optimizeAffineValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }
    IRSlotsFromASTType inner = recurse(type.getin());
    remoteSlot(
        new IRCellS(
            IRSlotTree.type(reqs, inner.activeRemoteTree, IRSlotTree.of(new IRSessionS()))));
  }

  @Override
  public void visit(ASTUsageLT type) {
    IRTypeFlagRequisites reqs = IRIsDroppableChecker.check(compiler, env, type.getin(), false);
    if (!compiler.optimizeAffineValue.get()) {
      reqs = IRTypeFlagRequisites.impossible();
    }
    IRSlotsFromASTType inner = recurse(type.getin());
    localSlot(
        new IRCellS(IRSlotTree.type(reqs, inner.activeLocalTree, IRSlotTree.of(new IRSessionS()))));
  }
}
