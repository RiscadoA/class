package pt.inescid.cllsj.compiler.anl;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.instruction.IRInstruction;

public class AnlFlowLocation {
  private AnlFlow flow;
  private int index;
  private boolean removed = false;
  private static int nextUnknown = 0;

  public AnlFlowLocation(AnlFlow flow, int index) {
    this.flow = flow;
    this.index = index;
  }

  public static AnlFlowLocation unknown() {
    return new AnlFlowLocation(null, -1 - nextUnknown++);
  }

  public AnlFlow getFlow() {
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

  public AnlFlowState getPreviousState() {
    AnlFlow flow = getFlow();
    int index = getIndex();
    return flow.getStates().get(index);
  }

  public AnlFlowState getNextState() {
    AnlFlow flow = getFlow();
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
      sb.append(flow.getBlock().getLocation());
      sb.append(":").append(index);
    }
    return sb.toString();
  }

  public void move(AnlFlow newFlow, int newIndex) {
    this.removed = false;
    this.flow = newFlow;
    this.index = newIndex;
  }

  public void markRemoved() {
    this.removed = true;
  }

  public IRInstruction getInstruction() {
    return getFlow().getBlock().get(getIndex());
  }

  // Returns the predecessor location in the flow, if there is one and only one.
  public Optional<AnlFlowLocation> getPredecessor() {
    if (getIndex() > 0) {
      return Optional.of(getFlow().getLocation(getIndex() - 1));
    } else if (getFlow().getSources().size() == 1) {
      return getFlow().getSources().stream().findFirst().map(f -> f.getLocations().getLast());
    } else {
      return Optional.empty();
    }
  }

  // Returns the successor location in the flow, if there is one and only one.
  public Optional<AnlFlowLocation> getSuccessor() {
    if (getIndex() < getFlow().getBlock().size() - 1) {
      return Optional.of(getFlow().getLocation(getIndex() + 1));
    } else if (getFlow().getTargets().size() == 1) {
      return getFlow().getTargets().stream().findFirst().map(f -> f.getLocations().getFirst());
    } else {
      return Optional.empty();
    }
  }

  public void replaceInstruction(IRInstruction newInstruction) {
    getFlow().getBlock().set(getIndex(), newInstruction);
  }

  public void insertInstructionBefore(IRInstruction newInstruction) {
    getFlow().addInstruction(index, newInstruction, new AnlFlowLocation(getFlow(), index + 1));
  }

  public void insertInstructionAfter(IRInstruction newInstruction) {
    getFlow().addInstruction(index + 1, newInstruction, new AnlFlowLocation(getFlow(), index + 1));
  }

  public void moveInstructionBefore(AnlFlowLocation beforeLocation) {
    IRInstruction instruction = getInstruction();
    removeInstruction();
    beforeLocation.getFlow().addInstruction(beforeLocation.getIndex(), instruction, this);
  }

  public void moveInstructionAfter(AnlFlowLocation afterLocation) {
    IRInstruction instruction = getInstruction();
    removeInstruction();
    afterLocation.getFlow().addInstruction(afterLocation.getIndex() + 1, instruction, this);
  }

  public void removeInstruction() {
    for (int i = index + 1; i < getFlow().getBlock().size(); ++i) {
      getFlow().getLocation(i).move(getFlow(), i - 1);
    }
    getFlow().getBlock().remove(index);
    getFlow().getLocations().remove(index);
    getFlow().getStates().remove(index);
    markRemoved();
  }

  // Checks if there's a single path from this location to the given location.
  public boolean hasSinglePathUntil(AnlFlowLocation loc) {
    if (this == loc) {
      return true;
    }

    Optional<AnlFlowLocation> successor = getSuccessor();
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
  public void forEachBefore(Function<AnlFlowLocation, Boolean> consumer) {
    forEachBefore(consumer, true);
  }

  // Calls the function for each instruction occurring after this location,
  // starting from the instruction just after this one.
  //
  // If at a given point the flow has two possible successors, the function stops.
  // If the function returns false, the iteration stops.
  public void forEachAfter(Function<AnlFlowLocation, Boolean> consumer) {
    forEachAfter(consumer, true);
  }

  public void forEachBefore(Function<AnlFlowLocation, Boolean> consumer, boolean first) {
    Optional<AnlFlowLocation> predecessor = getPredecessor();
    if (!first && !consumer.apply(this)) {
      return;
    }
    if (predecessor.isPresent()) {
      predecessor.get().forEachBefore(consumer, false);
    }
  }

  public void forEachAfter(Function<AnlFlowLocation, Boolean> consumer, boolean first) {
    Optional<AnlFlowLocation> successor = getSuccessor();
    if (!first && !consumer.apply(this)) {
      return;
    }
    if (successor.isPresent()) {
      successor.get().forEachAfter(consumer, false);
    }
  }
}
