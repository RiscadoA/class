package pt.inescid.cllsj.compiler.ir.flow;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;

public class IRFlow {
  private IRBlock block;

  // Known state at each instruction
  private List<IRFlowState> states = new ArrayList<>();

  // Blocks which can lead to this flow
  private Set<IRFlow> sources = new HashSet<>();

  // Blocks which can execute any time after this one
  private Set<IRFlow> detached = new HashSet<>();

  // Blocks into which this one must diverge
  private Set<IRFlow> branches = new HashSet<>();

  public IRFlow(IRBlock block) {
    this.block = block;
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

  public void addState(IRFlowState state) {
    states.add(state);
  }

  public void addSource(IRFlow source) {
    sources.add(source);
  }

  public void addDetached(IRFlow detached) {
    this.detached.add(detached);
  }

  public void addBranch(IRFlow branch) {
    branches.add(branch);
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

    for (IRFlow branch : this.branches) {
      sb.append(branch.toString());
    }

    for (IRFlow detached : this.detached) {
      sb.append(detached.toString());
    }

    return sb.toString();
  }
}
