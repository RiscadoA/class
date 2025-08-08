package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;

import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.flow.IRFlow;
import pt.inescid.cllsj.compiler.ir.flow.IRFlowState;
import pt.inescid.cllsj.compiler.ir.instructions.*;

public class IROptimizer {
  private Map<String, Map<IRBlock, IRFlow>> processFlows = new HashMap<>();

  public void analyze(IRProgram program) {
    for (Map.Entry<String, IRProcess> e : program.getProcesses().entrySet()) {
      processFlows.put(e.getKey(), IRAnalyzer.analyze(e.getValue()));
    }
  }

  public void printProcessFlows(IRProgram program) {
    for (Map.Entry<String, Map<IRBlock, IRFlow>> e : processFlows.entrySet()) {
      IRProcess p = program.getProcesses().get(e.getKey());
      System.out.println(e.getKey() + ":");
      System.out.println(e.getValue().get(p.getEntry()));
      System.out.println();
    }
  }

  public void optimizeKnownJumps(IRProgram ir) {
    for (Map.Entry<String, IRProcess> e : ir.getProcesses().entrySet()) {
      if (!processFlows.containsKey(e.getKey())) {
        throw new IllegalStateException(
            "Analysis must be enabled to perform known jumps optimization");
      }
      optimizeKnownJumps(e.getValue(), processFlows.get(e.getKey()));
      removeUnreachableBlocks(e.getValue(), processFlows.get(e.getKey()));


      // Remove all references to the removed blocks
      Consumer<IRBlock> forEachBlock = block -> {
        for (int index = 0; index < block.getInstructions().size(); ++index) {
          IRInstruction instruction = block.getInstructions().get(index);
          if (instruction instanceof IRNewSession) {
            IRNewSession i = (IRNewSession) instruction;
            if (i.getLabel().isPresent() && !e.getValue().containsBlock(i.getLabel().get())) {
              i.removeLabel();
            }
          } else if (instruction instanceof IRFlip) {
            IRFlip i = (IRFlip) instruction;
            if (!e.getValue().containsBlock(i.getContLabel())) {
              block.getInstructions().set(index, new IRReturn(i.getRecord()));
            }
          }
        }
      };
      forEachBlock.accept(e.getValue().getEntry());
      e.getValue().getBlocks().forEach(forEachBlock);
    }
  }

  private void optimizeKnownJumps(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    while(true) {
      Optional<IRFlow> prev = Optional.empty();
      Optional<IRFlow> next1 = Optional.empty();
      
      for (IRFlow flow : flows.values()) {
        if (flow.getBranches().size() != 1) {
          continue;
        }
        IRFlow branch = flow.getBranches().stream().findFirst().get();
        if (branch.getSources().size() != 1) {
          continue;
        }

        // We can concatenate the two blocks
        prev = Optional.of(flow);
        next1 = Optional.of(branch);
        break;
      }

      if (prev.isEmpty() || next1.isEmpty()) {
        break;
      }
      final Optional<IRFlow> next = next1;

      concatFlows(ir, prev.get(), next.get());
      ir.getBlocks().remove(next.get().getBlock());
      flows.remove(next.get().getBlock());
    }
  }

  private void concatFlows(IRProcess ir, IRFlow prev, IRFlow next) {
    // Remove the last instruction on the current block
    IRInstruction last = prev.getBlock().getInstructions().removeLast();
    IRFlowState lastState = prev.getStates().removeLast();
    next.getStates().removeFirst();

    // If the last instruction in the block modified the end point count,
    // we need to go back and subtract the end point of all branches leading here.
    // We might also need to clean up some earlier instructions.
    if (last instanceof IRBranchOnValue || last instanceof IRJump) {
      // No need for special handling here
    } else if (last instanceof IRFlip) {
      // We need to look for the last time this record's continuation was set and
      // change it to the label stored in this flip (e.g., last IRFlip or IRNewSession).
      IRFlip i = (IRFlip) last;
      int recordLocation = lastState.getBoundRecord(i.getRecord()).getHeapLocation();
      replaceContinuation(ir, prev, recordLocation, i.getContLabel());
    } else if (last instanceof IRBranch) {
      IRBranch i = (IRBranch) last;
      if (i.getThen().getLabel().equals(next.getBlock().getLabel())) {
        subtractEndPoints(ir, prev, i.getOtherwise().getEndPoints());
      } else {
        subtractEndPoints(ir, prev, i.getThen().getEndPoints());
      }
    } else if (last instanceof IRPopTag) {
      IRPopTag i = (IRPopTag) last;
      int endPoints = 0;
      for (IRPopTag.Case c : i.getCases().values()) {
        if (!c.getLabel().equals(next.getBlock().getLabel())) {
          endPoints += c.getEndPoints();
        }
      }
      subtractEndPoints(ir, prev, endPoints);

      // This is a troublesome instruction.
      // If remove the pop tag, we would need to remove the push too, and, additionally, modify the type of the record
      // That is complicated and in real life, this optimization probably almost never happens.
      // So, we pop the tag anyway, and just omit the jump.
      i.getCases().clear();
      prev.getBlock().getInstructions().add(i);
      prev.getStates().add(lastState);
    } else if (last instanceof IRPopType) {
      IRPopType i = (IRPopType) last;
      i.removeNegativeLabel();
      i.removePositiveLabel();
      prev.getBlock().getInstructions().add(i);
      prev.getStates().add(lastState);
    } else if (last instanceof IRForward) {
      // We still want to execute the forward instruction, we just avoid the jump
      IRForward i = (IRForward) last;
      i.removeReturn();
      prev.getBlock().getInstructions().add(i);
      prev.getStates().add(lastState);
      subtractEndPoints(ir, prev, 1);
    } else if (last instanceof IRReturn) {
      // We're removing a single end point of the process.
      subtractEndPoints(ir, prev, 1);
    } else {
      throw new UnsupportedOperationException("Unexpected block ending instruction type: " + last.getClass().getSimpleName());
    }

    // Add the next block's instructions to the current block
    prev.getBlock().getInstructions().addAll(next.getBlock().getInstructions());
    prev.getStates().addAll(next.getStates());

    // Link with the outgoing edges of the next flow
    prev.removeBranch(next);
    for (IRFlow branch : next.getBranches()) {
      branch.removeSource(next);
      branch.addSource(prev);
      prev.addBranch(branch);
    }
    for (IRFlow detached : next.getDetached()) {
      detached.removeSource(next);
      detached.addSource(prev);
      prev.addDetached(detached);
    }
  }

  // Goes back in the flow graph searching for the last time the continuation was set
  // for the given record. Replaces the continuation label with the given label.
  private void replaceContinuation(IRProcess ir, IRFlow flow, int recordLocation, String label) {
    IRBlock block = flow.getBlock();
    for (int index = block.getInstructions().size() - 1; index >= 0; --index) {
      IRInstruction instruction = block.getInstructions().get(index);
        IRFlowState nextState = flow.getStates().get(index + 1);

      if (instruction instanceof IRNewSession) {
        IRNewSession i = (IRNewSession) instruction;
        if (nextState.getBoundRecord(i.getRecord()).getHeapLocation() == recordLocation) {
          i.setLabel(label);
          return;
        }
      } else if (instruction instanceof IRFlip) {
        IRFlip i = (IRFlip) instruction;
        if (nextState.getBoundRecord(i.getRecord()).getHeapLocation() == recordLocation) {
          i.setContLabel(label);
          return;
        } 
      }
    }

    for (IRFlow source : flow.getSources()) {
      replaceContinuation(ir, source, recordLocation, label);
    }
  }

  private void subtractEndPoints(IRProcess ir, IRFlow flow, int endPoints) {
    subtractEndPoints(ir, flow, endPoints, new HashSet<>());
  }

  // Decrements the end points of all branch instructions leading to the given flow
  private void subtractEndPoints(IRProcess ir, IRFlow flow, int endPoints, Set<IRFlow> visited) {
    if (!visited.add(flow)) {
      return;
    }

    if (flow.getSources().isEmpty()) {
      ir.subtractEndPoints(endPoints);
    }

    for (IRFlow source : flow.getSources()) {
      IRInstruction last = source.getBlock().getInstructions().getLast();
      if (last instanceof IRBranch) {
        IRBranch i = (IRBranch) last;
        if (i.getThen().getLabel().equals(flow.getBlock().getLabel())) {
          i.getThen().subtractEndPoints(endPoints);
        } else {
          i.getOtherwise().subtractEndPoints(endPoints);
        }
      } else if (last instanceof IRPopTag) {
        IRPopTag i = (IRPopTag) last;
        for (IRPopTag.Case c : i.getCases().values()) {
          if (c.getLabel().equals(flow.getBlock().getLabel())) {
            c.subtractEndPoints(endPoints);
          }
        }
      }

      subtractEndPoints(ir, source, endPoints);
    }
  }

  private void removeUnreachableBlocks(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    while(true) {
      Optional<IRBlock> toRemove = Optional.empty();

      for (IRBlock block : ir.getBlocks()) {
        if (!flows.containsKey(block) || flows.get(block).getSources().isEmpty()) {
          toRemove = Optional.of(block);
          break;
        }
      }

      if (toRemove.isEmpty()) {
        break;
      }

      ir.getBlocks().remove(toRemove.get());
      IRFlow flow = flows.remove(toRemove.get());
      if (flow != null) {
        for (IRFlow branch : flow.getBranches()) {
          branch.getSources().remove(flow);
        }
        for (IRFlow detached : flow.getDetached()) {
          detached.getSources().remove(flow);
        }
      }
    }
  }

  public void optimizeFlipForward(IRProgram ir) {
    for (IRProcess process : ir.getProcesses().values()) {
      optimizeFlipForward(process.getEntry());
      for (IRBlock block : process.getBlocks()) {
        optimizeFlipForward(block);
      }
    }
  }

  private void optimizeFlipForward(IRBlock ir) {
    int size = ir.getInstructions().size();
    if (size < 2) {
      return; // Not what we're looking for
    }
    IRInstruction maybeFlip = ir.getInstructions().get(size - 2);
    IRInstruction maybeForward = ir.getInstructions().get(size - 1);
    if (!(maybeFlip instanceof IRFlip && maybeForward instanceof IRForward)) {
      return; // Not what we're looking for
    }
    IRFlip flip = (IRFlip) maybeFlip;
    IRForward forward = (IRForward) maybeForward;
    if (flip.getRecord() != forward.getNegRecord()) {
      return; // Not what we're looking for
    }
    int x = forward.getNegRecord();
    int y = forward.getPosRecord();

    // We've found a block of the form:
    //
    //     flip(x)
    //     forward(-x, +y)
    //
    // Our goal is to avoid the flip entirely by executing the forward earlier
    // The tradeoff here is that we might need to allocate more memory for the merged record
    // What we gain is that we deallocate unnecessary memory earlier and avoid a costly flip.

    ir.getInstructions().removeLast();
    ir.getInstructions().removeLast();

    ir.add(new IRFlipForward(x, y));
  }
}
