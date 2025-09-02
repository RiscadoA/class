package pt.inescid.cllsj.compiler.ir;

import java.util.Optional;
import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.*;
import pt.inescid.cllsj.compiler.Compiler;
import pt.inescid.cllsj.compiler.ir.slot.*;

// From a given AST type, this visitor generates a IR slot corresponding to the root type,
// a IRSlotCombinations corresponding to all the local data requirements,
// and a IRSlotCombinations corresponding to all the remote data requirements.
public class IRSlotsFromASTType extends ASTTypeVisitor {
  private Compiler compiler;

  // Slot corresponding to the root type
  public Optional<IRSlot> slot = Optional.empty();

  // Possible slot combinations for the current negative segment of the type
  // I.e., what data is currently being read from the local side
  // This must be a IRSlotsCombinations as we may have choice types
  public IRSlotCombinations activeLocalCombinations = IRSlotCombinations.EMPTY;

  // Similar to above, but for the remote side (positive segment)
  // I.e., what data is currently being written to the remote side
  public IRSlotCombinations activeRemoteCombinations = IRSlotCombinations.EMPTY;

  // Possible slot combinations for all remaining negative segments of the type,
  // excluding the current one.
  // I.e., what data will be read from the local side in the future
  public IRSlotCombinations remainderLocalCombinations = IRSlotCombinations.EMPTY;

  // Similar to above, but for the remote side (positive segments)
  // I.e., what data will be written to the remote side in the future
  public IRSlotCombinations remainderRemoteCombinations = IRSlotCombinations.EMPTY;

  // Returns all local slot combinations, both active and remainder
  public IRSlotCombinations localCombinations() {
    return activeLocalCombinations.merge(remainderLocalCombinations);
  }

  // Returns all remote slot combinations, both active and remainder
  public IRSlotCombinations remoteCombinations() {
    return activeRemoteCombinations.merge(remainderRemoteCombinations);
  }

  // Returns all slot combinations, both local and remote, active and remainder
  public IRSlotCombinations combinations() {
    return localCombinations().merge(remoteCombinations());
  }

  public static IRSlotsFromASTType compute(Compiler compiler, ASTType type) {
    IRSlotsFromASTType visitor = new IRSlotsFromASTType();
    visitor.compiler = compiler;
    type.accept(visitor);
    return visitor;
  }

  private IRSlotsFromASTType recurse(ASTType type) {
    return compute(compiler, type);
  }

  private void localSlot(IRSlot slot) {
    this.slot = Optional.of(slot);
    activeLocalCombinations = IRSlotCombinations.of(slot);
  }

  private void remoteSlot(IRSlot slot) {
    this.slot = Optional.of(slot);
    activeRemoteCombinations = IRSlotCombinations.of(slot);
  }

  @Override
  public void visit(ASTBangT type) {
    remoteSlot(new IRExponentialS());
  }

  @Override
  public void visit(ASTBotT type) {}

  @Override
  public void visit(ASTCaseT type) {
    slot = Optional.of(new IRTagS());

    for (ASTType choice : type.getcases().values()) {
      IRSlotsFromASTType result = recurse(choice);

      activeRemoteCombinations =
          activeRemoteCombinations.merge(result.activeRemoteCombinations).prefix(slot.get());
      remainderLocalCombinations = remainderLocalCombinations.merge(result.localCombinations());
      remainderRemoteCombinations =
          remainderRemoteCombinations.merge(result.remainderRemoteCombinations);
    }
  }

  @Override
  public void visit(ASTCoRecT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTIdT type) {
    // TODO:
    // 1. if it is a recursion variable, stop here
    // 2. if it is not, generate a var slot
    throw new UnsupportedOperationException("Unimplemented method 'visit'");
  }

  @Override
  public void visit(ASTNotT type) {
    type.getin().accept(this);

    IRSlotCombinations swapActive = activeLocalCombinations;
    IRSlotCombinations swapRemainder = remainderLocalCombinations;
    activeLocalCombinations = activeRemoteCombinations;
    remainderLocalCombinations = remainderRemoteCombinations;
    activeRemoteCombinations = swapActive;
    remainderRemoteCombinations = swapRemainder;
  }

  @Override
  public void visit(ASTOfferT type) {
    slot = Optional.of(new IRTagS());

    for (ASTType choice : type.getcases().values()) {
      IRSlotsFromASTType result = recurse(choice);

      activeLocalCombinations =
          activeLocalCombinations.merge(result.activeLocalCombinations).prefix(slot.get());
      remainderLocalCombinations =
          remainderLocalCombinations.merge(result.remainderLocalCombinations);
      remainderRemoteCombinations = remainderRemoteCombinations.merge(result.remoteCombinations());
    }
  }

  @Override
  public void visit(ASTOneT type) {}

  @Override
  public void visit(ASTRecT type) {
    type.getin().accept(this);
  }

  @Override
  public void visit(ASTRecvT type) {
    slot = Optional.of(new IRSessionS());

    // Only the active segment is sent through the session
    IRSlotsFromASTType lhsResult = recurse(type.getlhs());
    activeLocalCombinations = lhsResult.activeLocalCombinations.prefix(slot.get());

    IRSlotsFromASTType rhsResult = recurse(type.getrhs());
    activeLocalCombinations = rhsResult.activeLocalCombinations.prefix(activeLocalCombinations);
    remainderLocalCombinations = rhsResult.remainderLocalCombinations;
    remainderRemoteCombinations = rhsResult.remoteCombinations();
  }

  @Override
  public void visit(ASTSendT type) {
    slot = Optional.of(new IRSessionS());

    // Only the active segment is sent through the session
    IRSlotsFromASTType lhsResult = recurse(type.getlhs());
    activeRemoteCombinations = lhsResult.activeRemoteCombinations.prefix(slot.get());

    IRSlotsFromASTType rhsResult = recurse(type.getrhs());
    activeRemoteCombinations = rhsResult.activeRemoteCombinations.prefix(activeRemoteCombinations);
    remainderLocalCombinations = rhsResult.localCombinations();
    remainderRemoteCombinations = rhsResult.remainderRemoteCombinations;
  }

  @Override
  public void visit(ASTWhyT type) {
    localSlot(new IRExponentialS());
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
