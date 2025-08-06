package pt.inescid.cllsj.compiler.ir.flow;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.IRBlock;

public class IRFlow {
  private IRBlock block;

  // Known state at each instruction
  private List<IRFlowState> states = new ArrayList<>();

  // Blocks which can lead to this flow
  private Set<IRFlow> sources = new HashSet<>();

  // Closure blocks which can run after this flow
  private Set<IRFlow> forks = new HashSet<>();

  // Blocks into which this one can diverge
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

  public Set<IRFlow> getForks() {
    return forks;
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

  public void addFork(IRFlow fork) {
    forks.add(fork);
  }

  public void addBranch(IRFlow branch) {
    branches.add(branch);
  }
}
