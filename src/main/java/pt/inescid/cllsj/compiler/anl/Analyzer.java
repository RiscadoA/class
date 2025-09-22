package pt.inescid.cllsj.compiler.anl;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.instruction.*;

// Visitor which analyses the IR of a given process and generates a control and data flow graph.
public class Analyzer extends IRInstructionVisitor {
  private Map<IRBlock, AnlFlow> flows = new HashMap<>();
  private AnlFlow flow;
  private AnlFlowState state;
  private IRProcess process;
  private AnlFlowLocation flowLoc;

  public static Map<IRProcessId, Map<IRBlock, AnlFlow>> analyze(IRProgram process) {
    Map<IRProcessId, Map<IRBlock, AnlFlow>> result = new HashMap<>();
    process.stream().forEach(p -> result.put(p.getId(), analyze(p)));
    return result;
  }

  public static Map<IRBlock, AnlFlow> analyze(IRProcess process) {
    Analyzer analyzer = new Analyzer(process);
    analyzer.visit(process.getEntry(), VisitType.DETACHED);
    return analyzer.flows;
  }

  private Analyzer(IRProcess process) {
    this.process = process;
    this.flow = new AnlFlow(process.getEntry());
    this.state = new AnlFlowState();
    this.flows.put(process.getEntry(), this.flow);
  }

  public AnlFlow getFlow() {
    return flow;
  }

  public AnlFlow getFlow(IRCodeLocation location) {
    return getFlow(process.getBlock(location));
  }

  public AnlFlow getFlow(IRBlock block) {
    AnlFlow flow;
    if (!flows.containsKey(block)) {
      flow = new AnlFlow(block);
      flows.put(block, flow);
    } else {
      flow = flows.get(block);
    }
    return flow;
  }

  private static enum VisitType {
    DETACHED,
    BRANCH,
    PENDING,
  }

  private void visitNextPending() {
    state.handleUnknownWrites(this);
    Optional<AnlFlowContinuation> cont = state.popPendingContinuation();
    if (cont.isPresent()) {
      visit(cont.get().getLocation(), VisitType.PENDING);
    }
  }

  private void visit(IRCodeLocation location, VisitType type) {
    visit(process.getBlock(location), type);
  }

  private void visit(IRBlock block, VisitType type) {
    // Create a new flow object (if we haven't passed through it yet).
    AnlFlow previousFlow = this.flow;
    AnlFlowState previousState = this.state;
    AnlFlow currentFlow = getFlow(block);
    this.flow = currentFlow;

    // Link the previous block with this one.
    if (previousFlow != currentFlow) {
      if (type == VisitType.PENDING) {
        previousFlow.addTarget(currentFlow);
      } else if (type == VisitType.DETACHED) {
        previousFlow.addDetached(currentFlow);
        previousFlow.addTarget(currentFlow);
      } else if (type == VisitType.BRANCH) {
        previousFlow.addBranch(currentFlow);
      }
      currentFlow.addSource(previousFlow);
    }

    // Mark that we've passed through this block.
    currentFlow.addTrace();

    // Visit each instruction in the block, one by one.
    int index = 0;
    currentFlow.addState(index, this, flowLoc, state);
    for (IRInstruction instruction : block.stream().toList()) {
      state = state.clone();
      flowLoc = currentFlow.getLocation(index);
      instruction.accept(this);
      currentFlow.addState(++index, this, flowLoc, state);
    }

    this.flow = previousFlow;
    this.state = previousState;
  }

  @Override
  public void visit(IRInitializeSession instr) {
    AnlSessionState session = state.session(instr.getSessionId());
    if (instr.getContinuation().isPresent()) {
      session.cont = Optional.of(new AnlFlowContinuation(instr.getContinuation().get(), flowLoc));
    } else {
      session.cont = Optional.empty();
    }
    session.data = Optional.of(instr.getContinuationData());
    session.remote = Optional.of(instr.getSessionId());
  }

  @Override
  public void visit(IRContinueSession instr) {
    AnlSessionState session = state.session(instr.getSessionId());
    Optional<AnlSessionState> remote = session.remote.map(id -> state.session(id));

    Optional<IRCodeLocation> oldCont = session.cont.map(c -> c.getLocation());
    session.cont = Optional.empty();
    AnlFlowContinuation newCont = new AnlFlowContinuation(instr.getContinuation(), flowLoc);

    // If the remote is known, set its continuation to our old continuation
    if (remote.isPresent()) {
      remote.get().cont = Optional.of(newCont);
    } else {
      // Otherwise, push our continuation to the pending stack
      state.pushPendingContinuation(this, newCont, false);
    }

    if (oldCont.isPresent()) {
      // If we had a known continuation, jump to it
      visit(oldCont.get(), VisitType.BRANCH);
    } else {
      // Unknown continuation
      visitNextPending();
    }
  }

  @Override
  public void visit(IRFinishSession instr) {
    AnlSessionState session = state.session(instr.getSessionId());
    Optional<IRCodeLocation> cont = session.cont.map(c -> c.getLocation());
    session.cont = Optional.empty();
    if (session.remote.isPresent()) {
      AnlSessionState remote = state.session(session.remote.get());
      remote.cont = Optional.empty();
      remote.data = Optional.empty();
      remote.remote = Optional.empty();
    }
    if (cont.isPresent()) {
      visit(cont.get(), VisitType.BRANCH);
    } else {
      // Unknown continuation, may write to unknown locations
      visitNextPending();
    }
  }

  @Override
  public void visit(IRBindSession instr) {
    AnlSessionState targetSession = state.session(instr.getTargetSessionId());
    Optional<IRSessionId> sourceSessionId =
        state.read(instr.getSourceLocation(), true).assumeSession().map(s -> s.id());

    if (sourceSessionId.isEmpty()) {
      // We're binding an unknown session
      // The data we bound will be written to by unknown writes
      targetSession.cont = Optional.empty();
      targetSession.data = Optional.empty();
      targetSession.remote = Optional.empty();
      state.markDataAsUnknown(this, instr.getLocalData());
    } else {
      // We're binding a known session
      AnlSessionState sourceSession = state.session(sourceSessionId.get());
      targetSession.cont = sourceSession.cont;
      targetSession.data = sourceSession.data;
      targetSession.remote = sourceSession.remote;
      sourceSession.data = Optional.of(instr.getLocalData());
      sourceSession.remote = Optional.of(instr.getTargetSessionId());
    }
  }

  @Override
  public void visit(IRWriteExpression instr) {
    state.write(this, instr.getLocation(), new AnlSlot.Unknown());
  }

  @Override
  public void visit(IRWriteScan instr) {
    state.write(this, instr.getLocation(), new AnlSlot.Unknown());
  }

  @Override
  public void visit(IRWriteSession instr) {
    AnlSessionState local = state.session(instr.getSessionId());

    if (local.remote.isEmpty()) {
      state.write(this, instr.getLocation(), new AnlSlot.Unknown());
    } else {
      AnlSessionState remote = state.session(local.remote.get());
      remote.cont = local.cont;
      remote.data = local.data;
      remote.remote = local.remote;
      state.write(this, instr.getLocation(), new AnlSlot.Session(local.remote.get()));
    }
  }

  @Override
  public void visit(IRWriteTag instr) {
    state.write(this, instr.getLocation(), new AnlSlot.Tag(instr.getTag()));
  }

  @Override
  public void visit(IRWriteType instr) {
    state.write(this, instr.getLocation(), new AnlSlot.Unknown());
  }

  @Override
  public void visit(IRMoveSlots instr) {
    state.write(
        this, instr.getLocation(), state.read(instr.getSourceLocation(), instr.getSlots(), true));
  }

  @Override
  public void visit(IRCloneSlots instr) {
    state.write(
        this, instr.getLocation(), state.read(instr.getSourceLocation(), instr.getSlots(), false));
  }

  @Override
  public void visit(IRDropSlots instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRPrint instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRPushTask instr) {
    state.pushPendingContinuation(this, instr.getLocation(), flowLoc, false);
  }

  @Override
  public void visit(IRPopTask instr) {
    visitNextPending();
  }

  @Override
  public void visit(IRCallProcess instr) {
    for (IRCallProcess.SessionArgument arg : instr.getSessionArguments()) {
      if (arg.isFromLocation()) {
        state.read(arg.getSourceSessionLocation(), true).markAsUnknown(this, state);
      } else {
        state.session(arg.getSourceSessionId()).markAsUnknown(this, state);
      }
    }
    for (IRCallProcess.DataArgument arg : instr.getDataArguments()) {
      state.markDataAsUnknown(this, arg.getSourceLocation());
    }
    visitNextPending();
  }

  @Override
  public void visit(IRWriteExponential instr) {
    state.write(this, instr.getLocation(), new AnlSlot.Unknown());
  }

  @Override
  public void visit(IRCallExponential instr) {
    AnlSessionState session = state.session(instr.getSessionId());
    session.cont = Optional.empty();
    session.data = Optional.empty();
  }

  @Override
  public void visit(IRForwardSessions instr) {
    AnlSessionState neg = state.session(instr.getNegId());
    AnlSessionState pos = state.session(instr.getPosId());
    AnlSessionState oldNeg = neg.clone();
    AnlSessionState oldPos = pos.clone();

    Optional<AnlSessionState> negRemote = oldNeg.remote.map(id -> state.session(id));
    Optional<AnlSessionState> posRemote = oldPos.remote.map(id -> state.session(id));

    if (negRemote.isPresent()) {
      negRemote.get().cont = oldPos.cont;
      negRemote.get().data = oldPos.data;
      negRemote.get().remote = oldPos.remote;
    }

    if (posRemote.isPresent()) {
      posRemote.get().cont = oldNeg.cont;
      posRemote.get().data = oldNeg.data;
      posRemote.get().remote = oldNeg.remote;
    } else {
      oldNeg.markAsUnknown(this, state);
    }

    if (instr.shouldJump()) {
      if (oldPos.cont.isPresent()) {
        visit(oldPos.cont.get().getLocation(), VisitType.BRANCH);
      } else {
        visitNextPending();
      }
    }
  }

  @Override
  public void visit(IRBranchTag instr) {
    Optional<AnlSlot.Tag> slot = state.read(instr.getLocation(), false).assumeTag();
    if (slot.isPresent()) {
      visit(instr.getCases().get(slot.get().tag()).getLocation(), VisitType.BRANCH);
    } else {
      for (IRBranch.Case c : instr.getCases()) {
        visit(c.getLocation(), VisitType.BRANCH);
      }
    }
  }

  @Override
  public void visit(IRBranchExpression instr) {
    visit(instr.getThen().getLocation(), VisitType.BRANCH);
    visit(instr.getOtherwise().getLocation(), VisitType.BRANCH);
  }

  @Override
  public void visit(IRBranchTypeFlag instr) {
    visit(instr.getThen().getLocation(), VisitType.BRANCH);
    visit(instr.getOtherwise().getLocation(), VisitType.BRANCH);
  }

  @Override
  public void visit(IRDeferDrop instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRSleep instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRPanic instr) {
    visitNextPending();
  }

  @Override
  public void visit(IRJump instr) {
    visit(instr.getLocation(), VisitType.BRANCH);
  }

  @Override
  public void visit(IRAcquireCell instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRReleaseCell instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRWriteCell instr) {
    state.write(this, instr.getLocation(), new AnlSlot.Unknown());
  }

  @Override
  public void visit(IRLockCell instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRUnlockCell instr) {
    // Irrelevant for analysis
  }

  @Override
  public void visit(IRLaunchThread instr) {
    state.pushPendingContinuation(this, instr.getLocation(), flowLoc, true);
  }
}
