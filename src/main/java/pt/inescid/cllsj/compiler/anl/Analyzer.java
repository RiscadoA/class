package pt.inescid.cllsj.compiler.anl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import pt.inescid.cllsj.compiler.ir.instruction.IRBlock;
import pt.inescid.cllsj.compiler.ir.instruction.IRInstructionVisitor;

// Visitor which analyses the IR of a given process and generates a control and data flow graph.
public class Analyzer extends IRInstructionVisitor {
  private Map<IRBlock, AnlFlow> flows = new HashMap<>();
  private IRFlow flow;
  private IRFlowState state;
  private IRProcess process;
  private IRFlowLocation location;

  public static Map<IRBlock, IRFlow> analyze(IRProcess process) {
    Analyzer analyzer = new Analyzer(process);
    for (int i = 0; i < process.getTypeVariableCount(); ++i) {
      analyzer.state.bindType(i, new IRFlowType(process.isTypeVariablePositive(i)));
    }
    for (int i = 0; i < process.getRecordArgumentCount(); ++i) {
      analyzer.state.bindRecord(i, analyzer.state.allocateRecord(IRFlowLocation.unknown()));
    }
    for (int i = 0; i < process.getExponentialArgumentCount(); ++i) {
      analyzer.state.bindExponential(i, analyzer.state.allocateExponential(Optional.empty()));
    }
    analyzer.visit(process.getEntry(), VisitType.DETACHED);
    return analyzer.flows;
  }

  private Analyzer(IRProcess process) {
    this.process = process;
    this.flow = new IRFlow(process.getEntry());
    this.state = new IRFlowState();
    this.flows.put(process.getEntry(), this.flow);
  }

  public IRFlow getFlow() {
    return flow;
  }

  public IRFlow getFlow(String label) {
    return getFlow(process.getBlock(label));
  }

  public IRFlow getFlow(IRBlock block) {
    IRFlow flow;
    if (!flows.containsKey(block)) {
      flow = new IRFlow(block);
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
    Optional<IRFlowContinuation> cont = state.popPendingContinuation();
    if (cont.isPresent()) {
      visit(cont.get().getLabel(), VisitType.PENDING);
    }
  }

  private void visit(String label, VisitType type) {
    visit(process.getBlock(label), type);
  }

  private void visit(IRBlock block, VisitType type) {
    // Create a new flow object (if we haven't passed through it yet).
    IRFlow previousFlow = this.flow;
    IRFlowState previousState = this.state;
    IRFlow currentFlow = getFlow(block);
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

    // Visit each instruction in the block, one by one.
    int index = 0;
    currentFlow.addState(index, this, location, state);
    for (IRInstruction instruction : block.getInstructions()) {
      state = state.clone();
      location = currentFlow.getLocation(index);
      instruction.accept(this);
      currentFlow.addState(++index, this, location, state);
    }

    this.flow = previousFlow;
    this.state = previousState;
  }

  @Override
  public void visit(IRInstruction instruction) {
    throw new UnsupportedOperationException(
        "Unsupported instruction: " + instruction.getClass().getSimpleName());
  }

  @Override
  public void visit(IRNewTask instruction) {
    state.pushPendingContinuation(this, instruction.getLabel(), location);
  }

  @Override
  public void visit(IRNextTask instruction) {
    visitNextPending();
  }

  @Override
  public void visit(IRNewThread instruction) {
    state.pushPendingContinuation(this, instruction.getLabel(), location);
  }

  @Override
  public void visit(IRNewSession instruction) {
    IRFlowRecord record = state.allocateRecord(location);
    record.doNewSession(instruction.getLabel().map(l -> new IRFlowContinuation(l, location)));
    state.bindRecord(instruction.getRecord(), record);
  }

  @Override
  public void visit(IRFreeSession instruction) {
    state.freeRecord(state.getBoundRecord(instruction.getRecord()));
    state.unbindRecord(instruction.getRecord());
  }

  @Override
  public void visit(IRCleanRecord instruction) {
    state.freeRecord(state.getBoundRecord(instruction.getRecord()));
    state.unbindRecord(instruction.getRecord());
  }

  @Override
  public void visit(IRFlip instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    Optional<IRFlowContinuation> cont =
        record.doFlip(new IRFlowContinuation(instruction.getContLabel(), location));
    if (cont.isPresent()) {
      // The continuation is known, we should flip to the next state.
      visit(cont.get().getLabel(), VisitType.BRANCH);
    } else {
      // Unknown continuation. Slots will be left in an unknown state.
      record.doReturn(); // Pretend we've returned from the continuation
      record.markSlotsUnknown(this, state);
      visit(instruction.getContLabel(), VisitType.DETACHED);
    }
  }

  @Override
  public void visit(IRReturn instruction) {
    Optional<IRFlowContinuation> cont = state.getBoundRecord(instruction.getRecord()).doReturn();
    if (cont.isPresent()) {
      visit(cont.get().getLabel(), VisitType.BRANCH);
    } else {
      visitNextPending();
    }
  }

  @Override
  public void visit(IRPushSession instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    IRFlowRecord argRecord = state.getBoundRecord(instruction.getArgRecord());
    Optional<Boolean> value = state.isValue(instruction.getValueRequisites());

    if (value.isEmpty()) {
      record.markSlotsUnknown(this, state);
      argRecord.markTotallyUnknown(this, state);
    } else if (value.get()) {
      if (argRecord.slotsAreKnown()) {
        int slots = argRecord.getSlotCount().get();
        for (int i = 0; i < slots; ++i) {
          record.doPush(this, state, argRecord.doPop(location));
        }
      } else {
        argRecord.markTotallyUnknown(this, state);
        record.markSlotsUnknown(this, state);
      }
      state.unbindRecord(instruction.getArgRecord());
      state.freeRecord(argRecord);
    } else {
      record.doPush(this, state, IRFlowSlot.record(location, argRecord.getIntroductionLocation()));
    }
  }

  @Override
  public void visit(IRPopSession instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    Optional<Boolean> value = state.isValue(instruction.getValueRequisites());

    if (value.isEmpty()) {
      record.markSlotsUnknown(this, state);
      state.bindRecord(instruction.getArgRecord(), state.allocateRecord(IRFlowLocation.unknown()));
    } else if (value.get()) {
      IRFlowRecord argRecord = state.allocateRecord(location);
      state.bindRecord(instruction.getArgRecord(), argRecord);

      Optional<Integer> slotCount =
          state.slotCount(process.getRecordType(instruction.getArgRecord()), record.getSlots());

      if (slotCount.isPresent()) {
        argRecord.doNewSession(Optional.empty());
        for (int i = 0; i < slotCount.get(); ++i) {
          argRecord.doPush(this, state, record.doPop(location));
        }
      } else {
        record.markSlotsUnknown(this, state);
      }
    } else {
      IRFlowSlot slot = record.doPop(location);
      if (slot.isKnownRecord()) {
        state.bindRecord(instruction.getArgRecord(), slot.getRecordIntroductionLocation());
      } else {
        state.bindRecord(
            instruction.getArgRecord(), state.allocateRecord(IRFlowLocation.unknown()));
      }
    }
  }

  @Override
  public void visit(IRPushExpression instruction) {
    IRFlowSlot slot = visit(instruction.getExpression());
    if (instruction.isExponential()) {
      IRFlowExponential exponential = state.allocateExponential(Optional.of(List.of(slot)));
      slot = IRFlowSlot.exponential(location, exponential.getHeapLocation());
    }
    state.getBoundRecord(instruction.getRecord()).doPush(this, state, slot);
  }

  @Override
  public void visit(IRPrint instruction) {
    visit(instruction.getExpression());
  }

  @Override
  public void visit(IRPushClose instruction) {
    state.getBoundRecord(instruction.getRecord()).doPush(this, state, IRFlowSlot.close(location));
  }

  @Override
  public void visit(IRPopClose instruction) {
    state.getBoundRecord(instruction.getRecord()).doPop(location);
  }

  @Override
  public void visit(IRPushUnfold instruction) {
    state.getBoundRecord(instruction.getRecord()).doPushUnfold();
  }

  @Override
  public void visit(IRPopUnfold instruction) {
    state.getBoundRecord(instruction.getRecord()).doPopUnfold();
  }

  @Override
  public void visit(IRPushTag instruction) {
    state
        .getBoundRecord(instruction.getRecord())
        .doPush(this, state, IRFlowSlot.tag(location, instruction.getTag()));
  }

  @Override
  public void visit(IRPopTag instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop(location);
    if (!instruction.getCases().isEmpty()) {
      if (slot.isKnownTag()) {
        int tag = slot.getTag();
        visit(instruction.getCases().get(tag).getLabel(), VisitType.BRANCH);
      } else {
        // We don't know the tag, we must visit all branches
        for (IRPopTag.Case c : instruction.getCases().values()) {
          visit(c.getLabel(), VisitType.BRANCH);
        }
      }
    }
  }

  @Override
  public void visit(IRPushCell instruction) {
    state.getBoundRecord(instruction.getRecord()).doPush(this, state, IRFlowSlot.cell(location));
    state.getBoundRecord(instruction.getArgRecord()).markTotallyUnknown(this, state);
  }

  @Override
  public void visit(IRPutCell instruction) {
    state.getBoundRecord(instruction.getRecord()).doPush(this, state, IRFlowSlot.cell(location));
    state.getBoundRecord(instruction.getArgRecord()).markTotallyUnknown(this, state);
  }

  @Override
  public void visit(IRTakeCell instruction) {
    state.getBoundRecord(instruction.getRecord()).doPop(location);
    state.bindRecord(instruction.getArgRecord(), state.allocateRecord(location));
  }

  @Override
  public void visit(IRPushExponential instruction) {
    IRFlowExponential exponential = state.getBoundExponential(instruction.getExponential());
    IRFlowSlot slot = IRFlowSlot.exponential(location, exponential.getHeapLocation());
    state.getBoundRecord(instruction.getRecord()).doPush(this, state, slot);
  }

  @Override
  public void visit(IRPopExponential instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop(location);
    if (slot.isKnownExponential()) {
      state.bindExponential(instruction.getArgExponential(), slot.getExponentialHeapLocation());
    } else {
      state.bindExponential(
          instruction.getArgExponential(), state.allocateExponential(Optional.empty()));
    }
  }

  @Override
  public void visit(IRPushType instruction) {
    IRFlowType type =
        new IRFlowType(
            instruction.getType(), instruction.isPositive(), instruction.getValueRequisites());
    state
        .getBoundRecord(instruction.getRecord())
        .doPush(this, state, IRFlowSlot.type(location, type));
  }

  @Override
  public void visit(IRPopType instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop(location);
    if (slot.isKnownType()) {
      state.bindType(instruction.getArgType(), slot.getType());
    }
    if (slot.isKnownType() && slot.getType().isPositive().isPresent()) {
      if (slot.getType().isPositive().get()) {
        if (instruction.getPositive().isPresent()) {
          visit(instruction.getPositive().get().getLabel(), VisitType.BRANCH);
        }
      } else {
        if (instruction.getNegative().isPresent()) {
          visit(instruction.getNegative().get().getLabel(), VisitType.BRANCH);
        }
      }
    } else {
      if (instruction.getNegative().isPresent()) {
        visit(instruction.getNegative().get().getLabel(), VisitType.BRANCH);
      }
      if (instruction.getPositive().isPresent()) {
        visit(instruction.getPositive().get().getLabel(), VisitType.BRANCH);
      }
    }
  }

  @Override
  public void visit(IRNewExponential instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    IRFlowExponential exponential;

    IRFlowSlot slot = record.doPop(location);
    if (slot.isValue() && record.getSlotCount().get() == 0) {
      exponential = state.allocateExponential(Optional.of(List.of(slot)));
    } else {
      slot.markLost(this, state);
      exponential = state.allocateExponential(Optional.empty());
    }

    state.unbindRecord(instruction.getRecord());
    state.freeRecord(record);
    state.bindExponential(instruction.getExponential(), exponential);
  }

  @Override
  public void visit(IRCallExponential instruction) {
    IRFlowExponential exponential = state.getBoundExponential(instruction.getExponential());
    IRFlowRecord record = state.allocateRecord(location);
    state.bindRecord(instruction.getArgRecord(), record);

    if (exponential.hasKnownValue()) {
      record.doNewSession(Optional.empty());
      for (IRFlowSlot slot : exponential.getValue()) {
        record.doPush(this, state, slot);
      }
    }
  }

  @Override
  public void visit(IRCallProcess instruction) {
    for (IRCallProcess.LinearArgument arg : instruction.getLinearArguments()) {
      state.getBoundRecord(arg.getSourceRecord()).markTotallyUnknown(this, state);
    }
    visitNextPending();
  }

  @Override
  public void visit(IRForward instruction) {
    IRFlowRecord negRecord = state.getBoundRecord(instruction.getNegRecord());
    IRFlowRecord posRecord = state.getBoundRecord(instruction.getPosRecord());

    if (negRecord.slotsAreKnown()) {
      int slotCount = negRecord.getSlotCount().get();
      for (int i = 0; i < slotCount; ++i) {
        posRecord.doPush(this, state, negRecord.doPop(location));
      }
    } else {
      posRecord.markSlotsUnknown(this, state);
    }

    Optional<IRFlowContinuation> posRecordCont = posRecord.doFlip(negRecord.doReturn());

    state.freeRecord(negRecord);
    state.bindRecord(instruction.getNegRecord(), state.getBoundRecord(instruction.getPosRecord()));

    if (instruction.shouldReturn()) {
      if (posRecordCont.isPresent()) {
        visit(posRecordCont.get().getLabel(), VisitType.BRANCH);
      } else {
        visitNextPending();
      }
    }
  }

  @Override
  public void visit(IRIncRefCell instruction) {}

  @Override
  public void visit(IRDecRefCell instruction) {}

  @Override
  public void visit(IRIncRefExponential instruction) {}

  @Override
  public void visit(IRDecRefExponential instruction) {}

  @Override
  public void visit(IRDetachExponential instruction) {
    state.unbindExponential(instruction.getExponential());
  }

  @Override
  public void visit(IRSleep instruction) {}

  @Override
  public void visit(IRPanic instruction) {
    visitNextPending();
  }

  @Override
  public void visit(IRScan instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());

    boolean isExponential = false;
    IRType type = instruction.getType();
    if (type instanceof IRExponentialT) {
      isExponential = true;
      type = ((IRExponentialT) type).getInner();
    }

    IRFlowSlot slot;
    if (type instanceof IRIntT) {
      slot = IRFlowSlot.integer(location);
    } else if (type instanceof IRStringT) {
      slot = IRFlowSlot.string(location);
    } else if (type instanceof IRBoolT) {
      slot = IRFlowSlot.bool(location);
    } else {
      throw new IllegalArgumentException("Unsupported scan type: " + instruction.getType());
    }

    if (isExponential) {
      IRFlowExponential exponential = state.allocateExponential(Optional.of(List.of(slot)));
      slot = IRFlowSlot.exponential(location, exponential.getHeapLocation());
    }

    record.doPush(this, state, slot);
  }

  @Override
  public void visit(IRBranch instruction) {
    visit(instruction.getExpression());
    visit(instruction.getThen().getLabel(), VisitType.BRANCH);
    visit(instruction.getOtherwise().getLabel(), VisitType.BRANCH);
  }

  @Override
  public void visit(IRJump instruction) {
    visit(instruction.getLabel(), VisitType.BRANCH);
  }

  private IRFlowSlot visit(IRExpression expression) {
    ExpressionVisitor visitor = new ExpressionVisitor();
    expression.accept(visitor);
    return visitor.slot;
  }

  private class ExpressionVisitor extends IRExpressionVisitor {
    private IRFlowSlot slot = IRFlowSlot.unknown(location);

    private void setSlot(IRType type) {
      if (type instanceof IRStringT) {
        slot = IRFlowSlot.string(location);
      } else if (type instanceof IRIntT) {
        slot = IRFlowSlot.integer(location);
      } else if (type instanceof IRBoolT) {
        slot = IRFlowSlot.bool(location);
      } else {
        throw new UnsupportedOperationException(
            "Unsupported type: " + type.getClass().getSimpleName());
      }
    }

    @Override
    public void visit(IRExpression expr) {
      throw new UnsupportedOperationException(
          "Unsupported expression: " + expr.getClass().getSimpleName());
    }

    @Override
    public void visit(IRExponentialVar expr) {
      IRFlowExponential exponential = state.getBoundExponential(expr.getExponential());
      if (exponential.hasKnownValue() && exponential.getValue().size() == 1) {
        slot = exponential.getValue().get(0);
      }
    }

    @Override
    public void visit(IRVar expr) {
      slot = state.getBoundRecord(expr.getRecord()).doPop(location);
    }

    @Override
    public void visit(IRString expr) {
      slot = IRFlowSlot.string(location);
    }

    @Override
    public void visit(IRBool expr) {
      slot = IRFlowSlot.bool(location);
    }

    @Override
    public void visit(IRInt expr) {
      slot = IRFlowSlot.integer(location);
    }

    @Override
    public void visit(IRNot expr) {
      expr.getInner().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IROr expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRAnd expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRGt expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRLt expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IREq expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRDiv expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRMul expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRSub expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }

    @Override
    public void visit(IRAdd expr) {
      expr.getLhs().accept(this);
      expr.getRhs().accept(this);
      setSlot(expr.getType());
    }
  }
}
