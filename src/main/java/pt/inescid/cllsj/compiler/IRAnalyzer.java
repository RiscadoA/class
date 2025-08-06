package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.flow.*;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRFreeSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRNextTask;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPopValue;
import pt.inescid.cllsj.compiler.ir.instructions.IRPrint;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushExpression;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRPushValue;
import pt.inescid.cllsj.compiler.ir.instructions.IRReturn;
import pt.inescid.cllsj.compiler.ir.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.type.IRType;

// Visitor which analyses the IR of a given process and generates a control and data flow graph.
public class IRAnalyzer extends IRInstructionVisitor {
  private Map<IRBlock, IRFlow> flows = new HashMap<>();
  private Optional<IRFlow> flow = Optional.empty();
  private IRFlowState state = new IRFlowState();
  private IRProcess process;

  public static IRFlow analyze(IRProcess process) {
    IRAnalyzer analyzer = new IRAnalyzer(process);
    return analyzer.visit(process.getEntry());
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
      visit(cont.get());
    }
  }

  private IRFlow visit(String label) {
    return visit(process.getBlock(label));
  }

  private IRFlow visit(IRBlock block) {
    if (flows.containsKey(block)) {
      throw new IllegalStateException("Block " + block.getLabel() + " has already been visited");
    }

    // Create a new flow object and mark the previous one as its source.
    Optional<IRFlow> previousFlow = this.flow;
    IRFlowState previousState = this.state;
    IRFlow currentFlow = new IRFlow(block);
    flows.put(block, currentFlow);
    if (previousFlow.isPresent()) {
      currentFlow.addSource(previousFlow.get());
    }

    // Visit each instruction in the block, one by one.
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
  public void visit(IRNextTask instruction) {
    visitNextPending();
  }

  @Override
  public void visit(IRNewSession instruction) {
    state.record(instruction.getRecord()).doNewSession(Optional.of(instruction.getLabel()));
  }

  @Override
  public void visit(IRFreeSession instruction) {}

  @Override
  public void visit(IRFlip instruction) {
    Optional<String> cont =
        state.record(instruction.getRecord()).doFlip(instruction.getContLabel());
    if (cont.isPresent()) {
      // The continuation is known, we should flip to the next state.
      visit(cont.get());
    } else {
      // Unknown continuation. Slots will be left in an unknown state.
      // Continue analyzing on the flip's continuation.
      state.record(instruction.getRecord()).markTotallyUnknown(state);
      visit(instruction.getContLabel());
    }
  }

  @Override
  public void visit(IRReturn instruction) {
    Optional<String> cont = state.record(instruction.getRecord()).doReturn();
    if (cont.isPresent()) {
      visit(cont.get());
    } else {
      visitNextPending();
    }
  }

  @Override
  public void visit(IRPushSession instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
    state.record(instruction.getRecord()).doPush(IRFlowSlot.record(record));
  }

  @Override
  public void visit(IRPopSession instruction) {
    Optional<IRFlowSlot> slot = state.record(instruction.getRecord()).doPop();
    if (slot.isPresent() && slot.get().isKnownRecord()) {
      state.bindRecord(instruction.getArgRecord(), slot.get().getRecord());
    }
  }

  @Override
  public void visit(IRPushValue instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
    IRFlowRecord argRecord = state.record(instruction.getArgRecord());
    Optional<Integer> slotCount = slotCount(argRecord.getIndex());
    if (argRecord.slotsAreKnown() && slotCount.isPresent()) {
      for (int i = 0; i < slotCount.get(); ++i) {
        record.doPush(argRecord.doPop().orElseThrow());
      }
    } else {
      argRecord.markTotallyUnknown(state);
      record.markSlotsUnknown(state);
    }
  }

  @Override
  public void visit(IRPopValue instruction) {
    IRFlowRecord record = state.record(instruction.getRecord());
    IRFlowRecord argRecord = state.record(instruction.getArgRecord());
    Optional<Integer> slotCount = slotCount(instruction.getArgRecord());

    if (record.slotsAreKnown() && slotCount.isPresent()) {
      for (int i = 0; i < slotCount.get(); ++i) {
        argRecord.doPush(record.doPop().orElseThrow());
      }
    }
    argRecord.doNewSession(Optional.empty());
  }

  @Override
  public void visit(IRPushExpression instruction) {
    state.record(instruction.getRecord()).doPush(IRFlowSlot.unknown());
  }

  @Override
  public void visit(IRPrint instruction) {}
}
