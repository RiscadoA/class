package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.flow.*;
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.type.*;

// Visitor which analyses the IR of a given process and generates a control and data flow graph.
public class IRAnalyzer extends IRInstructionVisitor {
  private Map<IRBlock, IRFlow> flows = new HashMap<>();
  private Optional<IRFlow> flow = Optional.empty();
  private IRFlowState state = new IRFlowState();
  private IRProcess process;

  public static IRFlow analyze(IRProcess process) {
    IRAnalyzer analyzer = new IRAnalyzer(process);
    for (int i = 0; i < process.getTypeVariableCount(); ++i) {
      analyzer.state.bindType(i, new IRFlowType(process.isTypeVariablePositive(i)));
    }
    for (int i = 0; i < process.getRecordArgumentCount(); ++i) {
      analyzer.state.bindRecord(i, analyzer.state.allocateRecord());
    }
    for (int i = 0; i < process.getExponentialArgumentCount(); ++i) {
      analyzer.state.bindExponential(i, analyzer.state.allocateExponential(Optional.empty()));
    }
    return analyzer.visit(process.getEntry(), true);
  }

  private IRAnalyzer(IRProcess process) {
    this.process = process;
  }

  private void visitNextPending() {
    Optional<String> cont = state.popPendingContinuation();
    if (cont.isPresent()) {
      visit(cont.get(), true);
    }
  }

  private IRFlow visit(String label, boolean detached) {
    return visit(process.getBlock(label), detached);
  }

  private IRFlow visit(IRBlock block, boolean detached) {
    // Create a new flow object (if we haven't passed through it yet).
    Optional<IRFlow> previousFlow = this.flow;
    IRFlowState previousState = this.state;
    IRFlow currentFlow;
    if (!flows.containsKey(block)) {
      currentFlow = new IRFlow(block);
      flows.put(block, currentFlow);
    } else {
      // We've already passed through the block, we'll need to merge states
      currentFlow = flows.get(block);
    }
    this.flow = Optional.of(currentFlow);

    // Link the previous block with this one.
    if (previousFlow.isPresent()) {
      if (detached) {
        previousFlow.get().addDetached(currentFlow);
      } else {
        previousFlow.get().addBranch(currentFlow);
      }
      currentFlow.addSource(previousFlow.get());
    }

    // Visit each instruction in the block, one by one.
    int index = 0;
    currentFlow.addState(index, state);
    for (IRInstruction instruction : block.getInstructions()) {
      state = state.clone();
      instruction.accept(this);
      currentFlow.addState(++index, state);
    }

    this.flow = previousFlow;
    this.state = previousState;
    return currentFlow;
  }

  @Override
  public void visit(IRInstruction instruction) {
    throw new UnsupportedOperationException(
        "Unsupported instruction: " + instruction.getClass().getSimpleName());
  }

  @Override
  public void visit(IRNewTask instruction) {
    state.pushPendingContinuation(instruction.getLabel());
  }

  @Override
  public void visit(IRNextTask instruction) {
    visitNextPending();
  }

  @Override
  public void visit(IRNewThread instruction) {
    state.pushPendingContinuation(instruction.getLabel());
  }

  @Override
  public void visit(IRNewSession instruction) {
    IRFlowRecord record = state.allocateRecord();
    record.doNewSession(Optional.of(instruction.getLabel()));
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
    Optional<String> cont = record.doFlip(instruction.getContLabel());
    if (cont.isPresent()) {
      // The continuation is known, we should flip to the next state.
      visit(cont.get(), false);
    } else {
      // Unknown continuation. Slots will be left in an unknown state.
      record.doReturn(); // Pretend we've returned from the continuation
      record.markSlotsUnknown(state);
      visit(instruction.getContLabel(), true);
    }
  }

  @Override
  public void visit(IRReturn instruction) {
    Optional<String> cont = state.getBoundRecord(instruction.getRecord()).doReturn();
    if (cont.isPresent()) {
      visit(cont.get(), false);
    } else {
      visitNextPending();
    }
  }

  @Override
  public void visit(IRPushSession instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    IRFlowRecord argRecord = state.getBoundRecord(instruction.getArgRecord());
    record.doPush(state, IRFlowSlot.record(argRecord.getHeapLocation()));
  }

  @Override
  public void visit(IRPopSession instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop();
    if (slot.isKnownRecord()) {
      state.bindRecord(instruction.getArgRecord(), slot.getRecordHeapLocation());
    } else {
      state.bindRecord(instruction.getArgRecord(), state.allocateRecord());
    }
  }

  @Override
  public void visit(IRPushValue instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    IRFlowRecord argRecord = state.getBoundRecord(instruction.getArgRecord());
    if (argRecord.slotsAreKnown()) {
      int slots = argRecord.getSlotCount().get();
      for (int i = 0; i < slots; ++i) {
        record.doPush(state, argRecord.doPop());
      }
    } else {
      argRecord.markTotallyUnknown(state);
      record.markSlotsUnknown(state);
    }
    state.unbindRecord(instruction.getArgRecord());
    state.freeRecord(argRecord);
  }

  @Override
  public void visit(IRPopValue instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    IRFlowRecord argRecord = state.allocateRecord();
    state.bindRecord(instruction.getArgRecord(), argRecord);

    Optional<Integer> slotCount = state.slotCount(process.getRecordType(instruction.getArgRecord()), record.getSlots());

    if (slotCount.isPresent()) {
      argRecord.doNewSession(Optional.empty());
      for (int i = 0; i < slotCount.get(); ++i) {
        argRecord.doPush(state, record.doPop());
      }
    } else {
      record.markSlotsUnknown(state);
    }
  }

  @Override
  public void visit(IRPushExpression instruction) {
    // TODO: pop slots from expression

    IRFlowSlot slot = IRFlowSlot.expression(instruction.getExpression());
    if (instruction.isExponential()) {
      IRFlowExponential exponential = state.allocateExponential(Optional.of(List.of(slot)));
      slot = IRFlowSlot.exponential(exponential.getHeapLocation());
    }
    state.getBoundRecord(instruction.getRecord()).doPush(state, slot);
  }

  @Override
  public void visit(IRPrint instruction) {
    // TODO: pop slots from expression
  }

  @Override
  public void visit(IRPushClose instruction) {
    state.getBoundRecord(instruction.getRecord()).doPush(state, IRFlowSlot.close());
  }

  @Override
  public void visit(IRPopClose instruction) {
    state.getBoundRecord(instruction.getRecord()).doPop();
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
    state.getBoundRecord(instruction.getRecord()).doPush(state, IRFlowSlot.tag(instruction.getTag()));
  }

  @Override
  public void visit(IRPopTag instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop();
    if (slot.isKnownTag()) {
      int tag = slot.getTag();
      visit(instruction.getCases().get(tag).getLabel(), false);
    } else {
      // We don't know the tag, we must visit all branches
      for (IRPopTag.Case c : instruction.getCases().values()) {
        visit(c.getLabel(), false);
      }
    }
  }

  @Override
  public void visit(IRPushCell instruction) {
    state.getBoundRecord(instruction.getRecord()).doPush(state, IRFlowSlot.cell());
    state.getBoundRecord(instruction.getArgRecord()).markTotallyUnknown(state);
  }

  @Override
  public void visit(IRPutCell instruction) {
    state.getBoundRecord(instruction.getRecord()).doPush(state, IRFlowSlot.cell());
    state.getBoundRecord(instruction.getArgRecord()).markTotallyUnknown(state);
  }

  @Override
  public void visit(IRTakeCell instruction) {
    state.getBoundRecord(instruction.getRecord()).doPop();
    state.bindRecord(instruction.getArgRecord(), state.allocateRecord());
  }

  @Override
  public void visit(IRPushExponential instruction) {
    IRFlowExponential exponential = state.getBoundExponential(instruction.getExponential());
    IRFlowSlot slot = IRFlowSlot.exponential(exponential.getHeapLocation());
    state.getBoundRecord(instruction.getRecord()).doPush(state, slot);
  }

  @Override
  public void visit(IRPopExponential instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop();
    if (slot.isKnownExponential()) {
      state.bindExponential(instruction.getArgExponential(), slot.getExponentialHeapLocation());
    } else {
      state.bindExponential(instruction.getArgExponential(), state.allocateExponential(Optional.empty()));
    }
  }

  @Override
  public void visit(IRPushType instruction) {
    IRFlowType type = new IRFlowType(instruction.getType(), instruction.isPositive(), instruction.getValueRequisites());
    state.getBoundRecord(instruction.getRecord()).doPush(state, IRFlowSlot.type(type));
  }

  @Override
  public void visit(IRPopType instruction) {
    IRFlowSlot slot = state.getBoundRecord(instruction.getRecord()).doPop();
    if (slot.isKnownType()) {
      state.bindType(instruction.getArgType(), slot.getType());
    }
    if (slot.isKnownType() && slot.getType().isPositive().isPresent()) {
      if (slot.getType().isPositive().get()) {
        visit(instruction.getPositiveLabel(), false);
      } else {
        visit(instruction.getNegativeLabel(), false);
      }
    } else {
      visit(instruction.getNegativeLabel(), false);
      visit(instruction.getPositiveLabel(), false);
    }
  }

  @Override
  public void visit(IRNewExponential instruction) {
    IRFlowRecord record = state.getBoundRecord(instruction.getRecord());
    IRFlowExponential exponential;

    IRFlowSlot slot = record.doPop();
    if (slot.isValue() && record.getSlotCount().get() == 0) {
      exponential = state.allocateExponential(Optional.of(List.of(slot)));
    } else {
      slot.markLost(state);
      exponential = state.allocateExponential(Optional.empty());
    }

    state.unbindRecord(instruction.getRecord());
    state.freeRecord(record);
    state.bindExponential(instruction.getExponential(), exponential);
  }

  @Override
  public void visit(IRCallExponential instruction) {
    IRFlowExponential exponential = state.getBoundExponential(instruction.getExponential());
    IRFlowRecord record = state.allocateRecord();
    state.bindRecord(instruction.getArgRecord(), record);

    if (exponential.hasKnownValue()) {
      record.doNewSession(Optional.empty());
      for (IRFlowSlot slot : exponential.getValue()) {
        record.doPush(state, slot);
      }
    }
  }

  @Override
  public void visit(IRCallProcess instruction) {
    for (IRCallProcess.LinearArgument arg : instruction.getLinearArguments()) {
      state.getBoundRecord(arg.getSourceRecord()).markTotallyUnknown(state);
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
        posRecord.doPush(state, negRecord.doPop());
      }
    } else {
      posRecord.markSlotsUnknown(state);
    }

    Optional<String> posRecordCont = posRecord.doFlip(negRecord.doReturn());

    state.freeRecord(negRecord);
    state.bindRecord(instruction.getNegRecord(), instruction.getPosRecord());

    if (posRecordCont.isPresent()) {
      visit(posRecordCont.get(), false);
    } else {
      visitNextPending();
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
  public void visit(IRPanic instruction) {}

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
      slot = IRFlowSlot.integer();
    } else if (type instanceof IRStringT) {
      slot = IRFlowSlot.string();
    } else if (type instanceof IRBoolT) {
      slot = IRFlowSlot.bool();
    } else {
      throw new IllegalArgumentException("Unsupported scan type: " + instruction.getType());
    }

    if (isExponential) {
      IRFlowExponential exponential = state.allocateExponential(Optional.of(List.of(slot)));
      slot = IRFlowSlot.exponential(exponential.getHeapLocation());
    }

    record.doPush(state, slot);
  }

  @Override
  public void visit(IRBranch instruction) {
    // TODO: pop slots from expression
    visit(instruction.getThen().getLabel(), false);
    visit(instruction.getOtherwise().getLabel(), false);
  }

  @Override
  public void visit(IRJump instruction) {
    visit(instruction.getLabel(), false);
  }

  @Override
  public void visit(IRBranchOnValue instruction) {
    Optional<Boolean> value = state.isValue(instruction.getRequisites());
    if (value.isEmpty()) {
      visit(instruction.getIsValue(), false);
      visit(instruction.getIsNotValue(), false);
    } else if (value.get()) {
      visit(instruction.getIsValue(), false);
    } else {
      visit(instruction.getIsNotValue(), false);
    }
  }
}
