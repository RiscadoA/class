package pt.inescid.cllsj.compiler.ir.flow;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import pt.inescid.cllsj.compiler.IRAnalyzer;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;

public class IRFlow {
  private IRBlock block;

  // Known state at each instruction
  private List<IRFlowState> states = new ArrayList<>();

  // Location of each instruction in the flow's block.
  private List<IRFlowLocation> locations = new ArrayList<>();

  // Blocks which can lead to this flow
  private Set<IRFlow> sources = new HashSet<>();

  // Blocks introduced here which can execute any time after this one
  private Set<IRFlow> detached = new HashSet<>();

  // Blocks into which this one must immediately diverge
  private Set<IRFlow> branches = new HashSet<>();

  // Blocks which were executed after this one
  // These don't necessarily match with any valid trace, but they are guaranteed to
  // represent one possible trace
  private Set<IRFlow> targets = new HashSet<>();

  public IRFlow(IRBlock block) {
    this.block = block;
    for (int i = 0; i < block.getInstructions().size(); ++i) {
      locations.add(new IRFlowLocation(this, i));
    }
  }

  public IRBlock getBlock() {
    return block;
  }

  public List<IRFlowState> getStates() {
    return states;
  }

  public Set<IRFlow> getSources() {
    return sources;
  }

  public Set<IRFlow> getDetached() {
    return detached;
  }

  public Set<IRFlow> getBranches() {
    return branches;
  }

  public Set<IRFlow> getTargets() {
    return targets;
  }

  public List<IRFlowLocation> getLocations() {
    return locations;
  }

  public IRFlowLocation getLocation(int index) {
    return locations.get(index);
  }

  public IRInstruction getInstruction(int index) {
    return block.getInstructions().get(index);
  }

  public void addState(int index, IRAnalyzer analyzer, IRFlowLocation location, IRFlowState state) {
    if (states.size() == index) {
      states.add(state);
    } else if (states.size() > index) {
      states.set(index, states.get(index).merge(analyzer, location, state));
    } else {
      throw new UnsupportedOperationException("States added in the wrong order");
    }
  }

  public void removeSource(IRFlow source) {
    sources.remove(source);
  }

  public void addSource(IRFlow source) {
    sources.add(source);
  }

  public void removeDetached(IRFlow detached) {
    this.detached.remove(detached);
  }

  public void addDetached(IRFlow detached) {
    this.detached.add(detached);
  }

  public void removeBranch(IRFlow branch) {
    branches.remove(branch);
  }

  public void removeTarget(IRFlow target) {
    removeDetached(target);
    removeBranch(target);
    targets.remove(target);
  }

  public void addBranch(IRFlow branch) {
    branches.add(branch);
    targets.add(branch);
  }

  public void addTarget(IRFlow target) {
    targets.add(target);
  }

  public void addInstruction(IRFlowLocation location) {
    IRInstruction instruction = location.getInstruction();
    location.move(this, block.getInstructions().size());
    locations.add(location);
    block.getInstructions().add(instruction);
  }

  public void addInstruction(int index, IRFlowLocation location) {
    addInstruction(index, location.getInstruction(), location);
  }

  public void addInstruction(int index, IRInstruction instruction, IRFlowLocation location) {
    location.move(this, index);
    locations.add(index, location);
    states.add(index, states.get(index));
    block.getInstructions().add(index, instruction);
    for (int i = index + 1; i < block.getInstructions().size(); ++i) {
      locations.get(i).move(this, i);
    }
  }

  public void removeLastInstruction() {
    getBlock().getInstructions().removeLast();
    getLocations().removeLast().markRemoved();
  }

  private void printLabels(StringBuffer sb, Set<IRFlow> flows) {
    if (flows.isEmpty()) {
      sb.append("none");
    } else {
      sb.append(
          String.join(
              ", ",
              flows.stream()
                  .map(IRFlow::getBlock)
                  .map(IRBlock::getLabel)
                  .map(l -> l == null ? "entry" : l)
                  .toList()));
    }
  }

  private void printState(StringBuffer sb, IRFlowState state) {
    state
        .toString()
        .lines()
        .forEach(
            line -> {
              sb.append("    [").append(line).append("]\n");
            });
  }

  @Override
  public String toString() {
    return toString(new HashSet<>());
  }

  public String toString(Set<IRFlow> shown) {
    if (!shown.add(this)) {
      return "";
    }

    StringBuffer sb = new StringBuffer();
    if (block.getLabel() != null) {
      sb.append(block.getLabel() + ":\n");
    }
    sb.append("    [sources: ");
    printLabels(sb, sources);
    sb.append("]\n");
    for (int i = 0; i < block.getInstructions().size(); ++i) {
      IRInstruction instruction = block.getInstructions().get(i);
      printState(sb, states.get(i));
      sb.append("        ");
      sb.append(instruction.toString());
      sb.append("\n");
    }

    printState(sb, states.getLast());
    if (!detached.isEmpty()) {
      sb.append("    [detached: ");
      printLabels(sb, detached);
      sb.append("]\n");
    }
    if (!branches.isEmpty()) {
      sb.append("    [branches: ");
      printLabels(sb, branches);
      sb.append("]\n");
    }
    if (!targets.isEmpty()) {
      sb.append("    [targets: ");
      printLabels(sb, targets);
      sb.append("]\n");
    }

    for (IRFlow target : this.targets) {
      sb.append(target.toString(shown));
    }

    return sb.toString();
  }
}
