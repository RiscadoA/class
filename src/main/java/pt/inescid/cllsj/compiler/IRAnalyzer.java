package pt.inescid.cllsj.compiler;

import java.util.HashMap;
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
    return analyzer.visit(process.getEntry(), true);
  }

  private IRAnalyzer(IRProcess process) {
    this.process = process;
  }

  private Optional<Integer> slotCount(IRType type) {
    if (type instanceof IRSessionT) {
      IRSessionT sessionType = (IRSessionT) type;
      Optional<Integer> contSlots = slotCount(sessionType.getCont());

      if (sessionType.getValueRequisites().mustBeValue()) {
        Optional<Integer> valueSlots = slotCount(sessionType.getArg());
        if (!contSlots.isPresent() || !valueSlots.isPresent()) {
          return Optional.empty();
        }
        return Optional.of(contSlots.get() + valueSlots.get());
      } else if (sessionType.getValueRequisites().canBeValue()) {
        return Optional.empty();
      } else {
        return contSlots;
      }
    } else {
      return Optional.of(1);
    }
  }

  private Optional<Integer> slotCount(int record) {
    return slotCount(process.getRecordType(record));
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
    if (flows.containsKey(block)) {
      throw new IllegalStateException("Block " + block.getLabel() + " has already been visited");
    }

    // Create a new flow object and mark the previous one as its source.
    Optional<IRFlow> previousFlow = this.flow;
    IRFlowState previousState = this.state;
    IRFlow currentFlow = new IRFlow(block);
    this.flow = Optional.of(currentFlow);
    flows.put(block, currentFlow);
    if (previousFlow.isPresent()) {
      if (detached) {
        previousFlow.get().addDetached(currentFlow);
      } else {
        previousFlow.get().addBranch(currentFlow);
      }
      currentFlow.addSource(previousFlow.get());
    }

    // Visit each instruction in the block, one by one.
    currentFlow.addState(state);
    for (IRInstruction instruction : block.getInstructions()) {
      state = state.clone();
      instruction.accept(this);
      currentFlow.addState(state);
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
    state.record(instruction.getRecord()).doNewSession(Optional.of(instruction.getLabel()));
  }

  @Override
  public void visit(IRFreeSession instruction) {
      state.unbindRecord(instruction.getRecord());
  }

  @Override
  public void visit(IRFlip instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
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
    Optional<String> cont = state.record(instruction.getRecord()).doReturn();
    if (cont.isPresent()) {
      visit(cont.get(), false);
    } else {
      state.unbindRecord(instruction.getRecord());
      visitNextPending();
    }
  }

  @Override
  public void visit(IRPushSession instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
    IRFlowRecord argRecord = state.record(instruction.getArgRecord());
    record.doPush(state, IRFlowSlot.record(argRecord));
  }

  @Override
  public void visit(IRPopSession instruction) {
    IRFlowSlot slot = state.record(instruction.getRecord()).doPop();
    if (slot.isKnownRecord()) {
      state.bindRecord(instruction.getArgRecord(), slot.getRecord());
    }
  }

  @Override
  public void visit(IRPushValue instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
    IRFlowRecord argRecord = state.record(instruction.getArgRecord());
    Optional<Integer> slotCount = slotCount(argRecord.getIndex());
    if (slotCount.isPresent()) {
      for (int i = 0; i < slotCount.get(); ++i) {
        record.doPush(state, argRecord.doPop());
      }
    } else {
      argRecord.markTotallyUnknown(state);
      record.markSlotsUnknown(state);
    }
    state.unbindRecord(instruction.getArgRecord());
  }

  @Override
  public void visit(IRPopValue instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
    IRFlowRecord argRecord = state.record(instruction.getArgRecord());
    Optional<Integer> slotCount = slotCount(instruction.getArgRecord());

    argRecord.doNewSession(Optional.empty());
    if (slotCount.isPresent()) {
      for (int i = 0; i < slotCount.get(); ++i) {
        argRecord.doPush(state, record.doPop());
      }
    }
  }

  @Override
  public void visit(IRPushExpression instruction) {
    IRFlowSlot slot;
    if (instruction.isExponential()) {
      slot = IRFlowSlot.exponential();
    } else {
      slot = IRFlowSlot.expression(instruction.getExpression());
    }
    state.record(instruction.getRecord()).doPush(state, slot);
  }

  @Override
  public void visit(IRPrint instruction) {}

  @Override
  public void visit(IRPushClose instruction) {
    state.record(instruction.getRecord()).doPush(state, IRFlowSlot.close());
  }

  @Override
  public void visit(IRPopClose instruction) {
    state.record(instruction.getRecord()).doPop();
  }

  @Override
  public void visit(IRPushUnfold instruction) {
    state.record(instruction.getRecord()).doPushUnfold();
  }

  @Override
  public void visit(IRPopUnfold instruction) {
    state.record(instruction.getRecord()).doPopUnfold();
  }

  @Override
  public void visit(IRPushTag instruction) {
    state.record(instruction.getRecord()).doPush(state, IRFlowSlot.tag(instruction.getTag()));
  }

  @Override
  public void visit(IRPopTag instruction) {
    IRFlowSlot slot = state.record(instruction.getRecord()).doPop();
    if (slot.isKnownTag()) {
      int tag = slot.getTag();
      visit(instruction.getCases().get(tag).getLabel(), false);
    } else {
      // We don't know the tag, we must visit both branches
      for (IRPopTag.Case c : instruction.getCases().values()) {
        visit(c.getLabel(), false);
      }
    }
  }

  @Override
  public void visit(IRCallProcess instruction) {
    for (IRCallProcess.LinearArgument arg : instruction.getLinearArguments()) {
      state.record(arg.getSourceRecord()).markTotallyUnknown(state);
    }
    visitNextPending();
  }
}
