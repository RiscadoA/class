package pt.inescid.cllsj.compiler.anl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.instruction.IRBlock;
import pt.inescid.cllsj.compiler.ir.instruction.IRInstruction;

public class AnlFlow {
  private IRBlock block;

  // Known state at each instruction
  private List<AnlFlowState> states = new ArrayList<>();

  // Location of each instruction in the flow's block.
  private List<AnlFlowLocation> locations = new ArrayList<>();

  // Blocks which can lead to this flow
  private Set<AnlFlow> sources = new HashSet<>();

  // Blocks introduced here which can execute any time after this one
  // Also stores how many traces the flow was detached on
  private Map<AnlFlow, Integer> detached = new HashMap<>();

  // Blocks into which this one must immediately diverge
  private Set<AnlFlow> branches = new HashSet<>();

  // Blocks which were executed after this one
  // These don't necessarily match with any valid trace, but they are guaranteed to
  // represent one possible trace
  private Set<AnlFlow> targets = new HashSet<>();

  // How many times this flow was traced
  private int traceCount = 0;

  public AnlFlow(IRBlock block) {
    this.block = block;
    for (int i = 0; i < block.size(); ++i) {
      locations.add(new AnlFlowLocation(this, i));
    }
  }

  public void addTrace() {
    traceCount += 1;
  }

  public int getTraceCount() {
    return traceCount;
  }

  public IRBlock getBlock() {
    return block;
  }

  public List<AnlFlowState> getStates() {
    return states;
  }

  public Set<AnlFlow> getSources() {
    return sources;
  }

  public Map<AnlFlow, Integer> getDetached() {
    return detached;
  }

  public boolean hasDetachesInAllTraces() {
    return detached.values().stream().anyMatch(v -> v >= traceCount);
  }

  public Set<AnlFlow> getBranches() {
    return branches;
  }

  public Set<AnlFlow> getTargets() {
    return targets;
  }

  public List<AnlFlowLocation> getLocations() {
    return locations;
  }

  public AnlFlowLocation getLocation(int index) {
    return locations.get(index);
  }

  public IRInstruction getInstruction(int index) {
    return block.get(index);
  }

  public void addState(int index, Analyzer analyzer, AnlFlowLocation location, AnlFlowState state) {
    if (states.size() == index) {
      states.add(state);
    } else if (states.size() > index) {
      states.set(index, states.get(index).merge(analyzer, location, state));
    } else {
      throw new UnsupportedOperationException("States added in the wrong order");
    }
  }

  public void removeSource(AnlFlow source) {
    sources.remove(source);
  }

  public void addSource(AnlFlow source) {
    sources.add(source);
  }

  public void removeDetached(AnlFlow detached) {
    this.detached.remove(detached);
  }

  public void addDetached(AnlFlow detached) {
    this.detached.compute(detached, (k, v) -> (v == null) ? 1 : v + 1);
  }

  public void removeBranch(AnlFlow branch) {
    branches.remove(branch);
  }

  public void removeTarget(AnlFlow target) {
    removeDetached(target);
    removeBranch(target);
    targets.remove(target);
  }

  public void addBranch(AnlFlow branch) {
    branches.add(branch);
    targets.add(branch);
  }

  public void addTarget(AnlFlow target) {
    targets.add(target);
  }

  public void addInstruction(AnlFlowLocation location) {
    IRInstruction instruction = location.getInstruction();
    location.move(this, block.size());
    locations.add(location);
    block.add(instruction);
  }

  public void addInstruction(int index, AnlFlowLocation location) {
    addInstruction(index, location.getInstruction(), location);
  }

  public void addInstruction(int index, IRInstruction instruction, AnlFlowLocation location) {
    location.move(this, index);
    locations.add(index, location);
    states.add(index, states.get(index));
    block.add(index, instruction);
    for (int i = index + 1; i < block.size(); ++i) {
      locations.get(i).move(this, i);
    }
  }

  public void removeLastInstruction() {
    getBlock().remove(getBlock().size() - 1);
    getLocations().removeLast().markRemoved();
  }

  public boolean guaranteedToRun() {
    return guaranteedToRun(new HashSet<>());
  }

  private boolean guaranteedToRun(Set<AnlFlow> seen) {
    if (seen.contains(this)) {
      return true;
    }
    seen.add(this);

    for (AnlFlow source : sources) {
      if (!source.getDetached().keySet().contains(this) && source.getBranches().size() > 1) {
        return false;
      }
      if (!source.guaranteedToRun(seen)) {
        return false;
      }
    }
    return true;
  }

  private void printLabels(StringBuffer sb, Map<AnlFlow, Integer> flows) {
    if (flows.isEmpty()) {
      sb.append("none");
    } else {
      sb.append(
          String.join(
              ", ",
              flows.entrySet().stream()
                .map(e -> e.getKey() + (e.getValue() > 1 ? " (x" + e.getValue() + ")" : ""))
                  .toList()));
    }
  }

  private void printLabels(StringBuffer sb, Set<AnlFlow> flows) {
    if (flows.isEmpty()) {
      sb.append("none");
    } else {
      sb.append(
          String.join(
              ", ",
              flows.stream()
                  .map(AnlFlow::getBlock)
                  .map(IRBlock::getLocation)
                  .map(Object::toString)
                  .toList()));
    }
  }

  private void printState(StringBuffer sb, AnlFlowState state) {
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

  public String toString(Set<AnlFlow> shown) {
    if (!shown.add(this)) {
      return "";
    }

    StringBuffer sb = new StringBuffer();
    sb.append("    [traces: " + traceCount + "]\n");
    sb.append(block.getLocation() + ":\n");
    sb.append("    [sources: ");
    printLabels(sb, sources);
    sb.append("]\n");
    for (int i = 0; i < block.size(); ++i) {
      IRInstruction instruction = block.get(i);
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

    for (AnlFlow target : this.targets) {
      sb.append(target.toString(shown));
    }

    return sb.toString();
  }
}
