package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.flow.IRFlow;
import pt.inescid.cllsj.compiler.ir.flow.IRFlowContinuation;
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
    }
  }

  private void optimizeKnownJumps(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    while (true) {
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
    // Get the last instruction on the current block
    IRInstruction last = prev.getBlock().getInstructions().getLast();
    prev.getStates().removeLast(); // Duplicated state
    Runnable removeLast =
        () -> {
          prev.removeLastInstruction();
          next.getStates().removeFirst();
        };

    // If the last instruction in the block modified the end point count,
    // we need to go back and subtract the end point of all branches leading here.
    // We might also need to clean up some earlier instructions.
    if (last instanceof IRJump) {
      // No need for special handling here, we simply remove the instruction
      removeLast.run();
    } else if (last instanceof IRFlip) {
      // We need to look for the last time this record's continuation was set and
      // change it to the label stored in this flip.
      IRFlip i = (IRFlip) last;
      IRFlowContinuation contBeforeFlip =
          prev.getStates().getLast().getBoundRecord(i.getRecord()).getContinuation().get();
      IRFlowContinuation contAfterFlip =
          next.getStates().getFirst().getBoundRecord(i.getRecord()).getContinuation().get();

      contBeforeFlip.replaceWritten(Optional.of(i.getContLabel()));
      contAfterFlip.setOverride(contBeforeFlip);
      removeLast.run();
    } else if (last instanceof IRBranch) {
      IRBranch i = (IRBranch) last;
      if (i.getThen().getLabel().equals(next.getBlock().getLabel())) {
        subtractEndPoints(ir, prev, i.getOtherwise().getEndPoints());
      } else {
        subtractEndPoints(ir, prev, i.getThen().getEndPoints());
      }
      removeLast.run();
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
      // If remove the tag pop, we would need to remove the push too, and, additionally, modify the
      // type of the record That is complicated and, in most cases, this optimization probably
      // almost
      // never happens anyway. So, we keep the instruction, and just omit the branching.
      i.getCases().clear();
    } else if (last instanceof IRPopType) {
      // Similarly to IRPopTag, we can remove the branch but not the instruction itself.
      IRPopType i = (IRPopType) last;
      if (i.getPositive().get().getLabel().equals(next.getBlock().getLabel())) {
        subtractEndPoints(ir, prev, i.getNegative().get().getEndPoints());
      } else {
        subtractEndPoints(ir, prev, i.getPositive().get().getEndPoints());
      }
      i.removeNegative();
      i.removePositive();
    } else if (last instanceof IRForward) {
      // Once again, we keep the forward but avoid the jump to the continuation.
      IRForward i = (IRForward) last;
      i.removeReturn();

      // We need to remove the label from the previous continuation writer, as it won't be used
      // anymore
      IRFlowContinuation contBeforeForward =
          prev.getStates().getLast().getBoundRecord(i.getPosRecord()).getContinuation().get();
      contBeforeForward.replaceWritten(Optional.empty());

      // We're removing a single end point of the process, if we didn't end up creating a new return
      // instruction
      if (!(contBeforeForward.getWriter().getInstruction() instanceof IRReturn)) {
        subtractEndPoints(ir, prev, 1);
      }
    } else if (last instanceof IRReturn) {
      // We're removing a single end point of the process.
      IRReturn i = (IRReturn) last;
      IRFlowContinuation contBeforeReturn =
          prev.getStates().getLast().getBoundRecord(i.getRecord()).getContinuation().get();
      contBeforeReturn.replaceWritten(Optional.empty());

      // We're removing a single end point of the process, if we didn't end up creating a new return
      // instruction.
      if (!(contBeforeReturn.getWriter().getInstruction() instanceof IRReturn)) {
        subtractEndPoints(ir, prev, 1);
      }
      removeLast.run();
    } else {
      throw new UnsupportedOperationException(
          "Unexpected block ending instruction type: " + last.getClass().getSimpleName());
    }

    // Add the next block's instructions to the current block
    for (int i = 0; i < next.getBlock().getInstructions().size(); ++i) {
      prev.addMovedInstruction(next.getInstruction(i), next.getLocation(i));
    }
    prev.getStates().addAll(next.getStates());

    // System.err.println(prev.getStates().get(0));
    // for (int i = 0; i < prev.getBlock().getInstructions().size(); ++i) {
    //   System.err.println(prev.getLocation(i));
    //   System.err.println("  " + prev.getBlock().getInstructions().get(i));
    //   System.err.println(prev.getStates().get(i + 1));
    // }

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
      } else if (last instanceof IRPopType) {
        IRPopType i = (IRPopType) last;
        if (i.getPositive().isPresent()
            && i.getPositive().get().getLabel().equals(flow.getBlock().getLabel())) {
          i.getPositive().get().subtractEndPoints(endPoints);
        } else if (i.getNegative().isPresent()
            && i.getNegative().get().getLabel().equals(flow.getBlock().getLabel())) {
          i.getNegative().get().subtractEndPoints(endPoints);
        }
      } else if (last instanceof IRPopTag) {
        IRPopTag i = (IRPopTag) last;
        for (IRPopTag.Case c : i.getCases().values()) {
          if (c.getLabel().equals(flow.getBlock().getLabel())) {
            c.subtractEndPoints(endPoints);
          }
        }
      }

      subtractEndPoints(ir, source, endPoints, visited);
    }
  }

  public void removeUnreachableBlocks(IRProgram ir) {
    for (Map.Entry<String, IRProcess> e : ir.getProcesses().entrySet()) {
      removeUnreachableBlocks(e.getValue(), processFlows.get(e.getKey()));
    }
  }

  private void removeUnreachableBlocks(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    while (true) {
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
    for (Map.Entry<String, IRProcess> e : ir.getProcesses().entrySet()) {
      Map<IRBlock, IRFlow> flows = processFlows.get(e.getKey());
      for (IRBlock flipBlock : e.getValue().getBlocksIncludingEntry()) {
        if (!flows.containsKey(flipBlock)) {
          continue;
        }
        IRFlow flipFlow = flows.get(flipBlock);

        IRInstruction maybeFlip = flipBlock.getInstructions().getLast();
        if (!(maybeFlip instanceof IRFlip)) {
          continue; // Not what we're looking for
        }
        IRFlip flip = (IRFlip) maybeFlip;
        IRBlock forwardBlock = e.getValue().getBlock(flip.getContLabel());
        IRInstruction maybeForward = forwardBlock.getInstructions().getFirst();
        if (!(maybeForward instanceof IRForward)) {
          continue; // Not what we're looking for
        }
        IRForward forward = (IRForward) maybeForward;
        if (flip.getRecord() != forward.getNegRecord() || !forward.shouldReturn()) {
          continue; // Not what we're looking for
        }
        int x = forward.getNegRecord();
        int y = forward.getPosRecord();

        // We've found two blocks of the form:
        //
        //     ...
        //     flip(x, cont)
        // cont:
        //     forward(-x, +y)
        //
        // Our goal is to avoid the flip entirely by executing the forward earlier
        // The tradeoff here is that we might need to allocate more memory for the merged record
        // What we gain is that we deallocate unnecessary memory earlier and avoid a costly flip.

        IRFlow forwardFlow = flows.get(forwardBlock);
        flipBlock.getInstructions().removeLast();
        flipBlock.getInstructions().add(new IRFlipForward(x, y));
        flipFlow.getStates().removeLast();
        flipFlow.getStates().add(forwardFlow.getStates().get(1));
        flipFlow.removeOutgoing(forwardFlow);
        forwardFlow.removeSource(flipFlow);
      }
    }
  }
}
