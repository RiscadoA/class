package pt.inescid.cllsj.compiler.ir.old.flow;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.old.instructions_old.IRInstruction;

public class IRFlowLocation {
  private IRFlow flow;
  private int index;
  private boolean removed = false;
  private static int nextUnknown = 0;

  public IRFlowLocation(IRFlow flow, int index) {
    this.flow = flow;
    this.index = index;
  }

  public static IRFlowLocation unknown() {
    return new IRFlowLocation(null, -1 - nextUnknown++);
  }

  public IRFlow getFlow() {
    if (removed) {
      throw new IllegalStateException("This flow location (" + this + ") has been removed");
    }
    return flow;
  }

  public int getIndex() {
    if (removed) {
      throw new IllegalStateException("This flow location (" + this + ") has been removed");
    }
    return index;
  }

  public boolean isKnown() {
    return index >= 0;
  }

  public IRFlowState getPreviousState() {
    IRFlow flow = getFlow();
    int index = getIndex();
    return flow.getStates().get(index);
  }

  public IRFlowState getNextState() {
    IRFlow flow = getFlow();
    int index = getIndex();
    return flow.getStates().get(index + 1);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (removed) {
      sb.append("removed ");
    }
    if (index < 0) {
      sb.append("unknown");
    } else {
      sb.append(Optional.ofNullable(flow.getBlock().getLabel()).orElse("entry"));
      sb.append(":").append(index);
    }
    return sb.toString();
  }

  public void move(IRFlow newFlow, int newIndex) {
    this.removed = false;
    this.flow = newFlow;
    this.index = newIndex;
  }

  public void markRemoved() {
    this.removed = true;
  }

  public IRInstruction getInstruction() {
    return getFlow().getBlock().getInstructions().get(getIndex());
  }

  // Returns the predecessor location in the flow, if there is one and only one.
  public Optional<IRFlowLocation> getPredecessor() {
    if (getIndex() > 0) {
      return Optional.of(getFlow().getLocation(getIndex() - 1));
    } else if (getFlow().getSources().size() == 1) {
      return getFlow().getSources().stream().findFirst().map(f -> f.getLocations().getLast());
    } else {
      return Optional.empty();
    }
  }

  // Returns the successor location in the flow, if there is one and only one.
  public Optional<IRFlowLocation> getSuccessor() {
    if (getIndex() < getFlow().getBlock().getInstructions().size() - 1) {
      return Optional.of(getFlow().getLocation(getIndex() + 1));
    } else if (getFlow().getTargets().size() == 1) {
      return getFlow().getTargets().stream().findFirst().map(f -> f.getLocations().getFirst());
    } else {
      return Optional.empty();
    }
  }

  public void replaceInstruction(IRInstruction newInstruction) {
    getFlow().getBlock().getInstructions().set(getIndex(), newInstruction);
  }

  public void insertInstructionBefore(IRInstruction newInstruction) {
    getFlow().addInstruction(index, newInstruction, new IRFlowLocation(getFlow(), index + 1));
  }

  public void insertInstructionAfter(IRInstruction newInstruction) {
    getFlow().addInstruction(index + 1, newInstruction, new IRFlowLocation(getFlow(), index + 1));
  }

  public void moveInstructionBefore(IRFlowLocation beforeLocation) {
    IRInstruction instruction = getInstruction();
    removeInstruction();
    beforeLocation.getFlow().addInstruction(beforeLocation.getIndex(), instruction, this);
  }

  public void moveInstructionAfter(IRFlowLocation afterLocation) {
    IRInstruction instruction = getInstruction();
    removeInstruction();
    afterLocation.getFlow().addInstruction(afterLocation.getIndex() + 1, instruction, this);
  }

  public void removeInstruction() {
    for (int i = index + 1; i < getFlow().getBlock().getInstructions().size(); ++i) {
      getFlow().getLocation(i).move(getFlow(), i - 1);
    }
    getFlow().getBlock().getInstructions().remove(index);
    getFlow().getLocations().remove(index);
    getFlow().getStates().remove(index);
    markRemoved();
  }

  // Checks if there's a single path from this location to the given location.
  public boolean hasSinglePathUntil(IRFlowLocation loc) {
    if (this == loc) {
      return true;
    }

    Optional<IRFlowLocation> successor = getSuccessor();
    if (successor.isEmpty()) {
      return false;
    }
    return successor.get().hasSinglePathUntil(loc);
  }

  // Calls the function for each instruction occurring before this location,
  // starting from the instruction just before this one.
  //
  // If at a given point the flow has two possible predecessors, the function stops.
  // If the function returns false, the iteration stops.
  public void forEachBefore(Function<IRFlowLocation, Boolean> consumer) {
    forEachBefore(consumer, true);
  }

  // Calls the function for each instruction occurring after this location,
  // starting from the instruction just after this one.
  //
  // If at a given point the flow has two possible successors, the function stops.
  // If the function returns false, the iteration stops.
  public void forEachAfter(Function<IRFlowLocation, Boolean> consumer) {
    forEachAfter(consumer, true);
  }

  public void forEachBefore(Function<IRFlowLocation, Boolean> consumer, boolean first) {
    Optional<IRFlowLocation> predecessor = getPredecessor();
    if (!first && !consumer.apply(this)) {
      return;
    }
    if (predecessor.isPresent()) {
      predecessor.get().forEachBefore(consumer, false);
    }
  }

  public void forEachAfter(Function<IRFlowLocation, Boolean> consumer, boolean first) {
    Optional<IRFlowLocation> successor = getSuccessor();
    if (!first && !consumer.apply(this)) {
      return;
    }
    if (successor.isPresent()) {
      successor.get().forEachAfter(consumer, false);
    }
  }
}
