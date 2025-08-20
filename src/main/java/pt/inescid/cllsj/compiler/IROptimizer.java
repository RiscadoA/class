package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRBlock;
import pt.inescid.cllsj.compiler.ir.IRProcess;
import pt.inescid.cllsj.compiler.ir.IRProgram;
import pt.inescid.cllsj.compiler.ir.IRTypeVisitor;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.flow.IRFlow;
import pt.inescid.cllsj.compiler.ir.flow.IRFlowContinuation;
import pt.inescid.cllsj.compiler.ir.flow.IRFlowLocation;
import pt.inescid.cllsj.compiler.ir.flow.IRFlowRecord;
import pt.inescid.cllsj.compiler.ir.flow.IRFlowState;
import pt.inescid.cllsj.compiler.ir.instructions.*;
import pt.inescid.cllsj.compiler.ir.type.IRBoolT;
import pt.inescid.cllsj.compiler.ir.type.IRCellT;
import pt.inescid.cllsj.compiler.ir.type.IRCloseT;
import pt.inescid.cllsj.compiler.ir.type.IRExponentialT;
import pt.inescid.cllsj.compiler.ir.type.IRFlipT;
import pt.inescid.cllsj.compiler.ir.type.IRIntT;
import pt.inescid.cllsj.compiler.ir.type.IRSessionT;
import pt.inescid.cllsj.compiler.ir.type.IRStringT;
import pt.inescid.cllsj.compiler.ir.type.IRTagT;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRTypeT;
import pt.inescid.cllsj.compiler.ir.type.IRVarT;

public class IROptimizer {
  private Map<String, Map<IRBlock, IRFlow>> processFlows = new HashMap<>();

  public void analyze(IRProgram program) {
    processFlows.clear();
    int processCount = program.getProcesses().size();
    int done = 0;
    for (Map.Entry<String, IRProcess> e : program.getProcesses().entrySet()) {
      System.err.println(
          "Analyzing process: " + e.getKey() + " (" + ++done + "/" + processCount + ")");
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
      Optional<IRFlow> next = Optional.empty();

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
        next = Optional.of(branch);
        break;
      }

      if (prev.isEmpty() || next.isEmpty()) {
        break;
      }

      concatFlows(ir, prev.get(), next.get());
      ir.getBlocks().remove(next.get().getBlock());
      flows.remove(next.get().getBlock());
    }
  }

  public void optimizeKnownEndPoints(IRProgram ir) {
    ir.forEachProcess((n, p) -> optimizeKnownEndPoints(p, processFlows.get(n)));
  }

  private void optimizeKnownEndPoints(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    for (IRFlow flow : flows.values()) {
      // We want to find the blocks which have at least one outgoing flow.
      // Those blocks should never be end points.
      // Thus, we skip any which do not have an outgoing flow - these are real endpoints.
      if (flow.getBranches().isEmpty() && flow.getDetached().isEmpty()) {
        continue;
      }

      IRInstruction instruction = flow.getBlock().getInstructions().getLast();
      boolean wasEndPoint;
      if (instruction instanceof IRCallProcess) {
        wasEndPoint = ((IRCallProcess) instruction).isEndPoint();
        ((IRCallProcess) instruction).removeEndPoint();
      } else if (instruction instanceof IRForward) {
        wasEndPoint = ((IRForward) instruction).isEndPoint();
        ((IRForward) instruction).removeEndPoint();
      } else if (instruction instanceof IRFlipForward) {
        wasEndPoint = ((IRFlipForward) instruction).isEndPoint();
        ((IRFlipForward) instruction).removeEndPoint();
      } else if (instruction instanceof IRReturn) {
        wasEndPoint = ((IRReturn) instruction).isEndPoint();
        ((IRReturn) instruction).removeEndPoint();
      } else if (instruction instanceof IRNextTask) {
        wasEndPoint = ((IRNextTask) instruction).isEndPoint();
        ((IRNextTask) instruction).removeEndPoint();
      } else {
        continue;
      }

      if (wasEndPoint) {
        modifyEndPoints(ir, flow.getBlock(), -1);
      }
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
      int unusedEndPoints = i.getEndPoints();
      if (i.getThen().getLabel().equals(next.getBlock().getLabel())) {
        unusedEndPoints -= i.getThen().getEndPoints();
      } else {
        unusedEndPoints -= i.getOtherwise().getEndPoints();
      }
      modifyEndPoints(ir, prev.getBlock(), -unusedEndPoints);
      removeLast.run();
    } else if (last instanceof IRPopTag) {
      IRPopTag i = (IRPopTag) last;
      int unusedEndPoints = i.getEndPoints();
      for (IRPopTag.Case c : i.getCases().values()) {
        if (c.getLabel().equals(next.getBlock().getLabel())) {
          unusedEndPoints -= c.getEndPoints();
          break;
        }
      }
      modifyEndPoints(ir, prev.getBlock(), -unusedEndPoints);

      // This is a troublesome instruction.
      // If remove the tag pop, we would need to remove the push too, and, additionally, modify the
      // type of the record That is complicated and, in most cases, this optimization probably
      // almost never happens anyway. So, we keep the instruction, and just omit the branching.
      i.getCases().clear();
    } else if (last instanceof IRPopType) {
      // Similarly to IRPopTag, we can remove the branch but not the instruction itself.
      IRPopType i = (IRPopType) last;
      int unusedEndPoints = i.getEndPoints();
      if (i.getPositive().get().getLabel().equals(next.getBlock().getLabel())) {
        unusedEndPoints -= i.getPositive().get().getEndPoints();
      } else {
        unusedEndPoints -= i.getNegative().get().getEndPoints();
      }
      modifyEndPoints(ir, prev.getBlock(), -unusedEndPoints);
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
        modifyEndPoints(ir, prev.getBlock(), -1);
      }
    } else if (last instanceof IRReturn) {
      // We're removing a single end point of the process.
      IRReturn i = (IRReturn) last;
      IRFlowContinuation contBeforeReturn =
          prev.getStates().getLast().getBoundRecord(i.getRecord()).getContinuation().get();
      contBeforeReturn.replaceWritten(Optional.empty());

      // We're removing a single end point of the process
      modifyEndPoints(ir, prev.getBlock(), -1);

      // We might have also just created a new return instruction,
      // in which case we'll need to add an end point there
      if (contBeforeReturn.getWriter().getInstruction() instanceof IRReturn) {
        modifyEndPoints(ir, contBeforeReturn.getWriter().getFlow().getBlock(), 1);
      }
      removeLast.run();
    } else {
      throw new UnsupportedOperationException(
          "Unexpected block ending instruction type: " + last.getClass().getSimpleName());
    }

    // Add the next block's instructions to the current block
    for (int i = 0; i < next.getBlock().getInstructions().size(); ++i) {
      prev.addInstruction(next.getLocation(i));
    }
    prev.getStates().addAll(next.getStates());

    // Link with the outgoing edges of the next flow
    prev.removeTarget(next);
    for (IRFlow branch : next.getBranches()) {
      branch.removeSource(next);
      branch.addSource(prev);
      prev.addBranch(branch);
    }
    for (IRFlow detached : next.getDetached()) {
      prev.addDetached(detached);
    }
    for (IRFlow target : next.getTargets()) {
      target.removeSource(next);
      target.addSource(prev);
      prev.addTarget(target);
    }
  }

  // Modifies the end points of all branch instructions leading to block.
  // If positive, increases the end point count, otherwise, decreases it.
  private void modifyEndPoints(IRProcess ir, IRBlock block, int endPoints) {
    modifyEndPoints(ir, block, endPoints, new HashSet<>());
  }

  private void modifyEndPoints(IRProcess ir, IRBlock block, int endPoints, Set<IRBlock> visited) {
    if (endPoints == 0) {
      return;
    }

    if (!visited.add(block)) {
      return;
    }

    // Entry block
    if (block.getLabel() == null) {
      ir.setEndPoints(ir.getEndPoints() + endPoints);
      return;
    }

    // Find blocks which reference this block
    for (IRBlock introducer : ir.getBlocksIncludingEntry()) {
      boolean referenced = false;
      for (IRInstruction instr : introducer.getInstructions()) {
        if (instr.usesLabel(block.getLabel())) {
          referenced = true;
          break;
        }
      }
      if (!referenced) {
        continue;
      }

      // We found one! Now we check if the last instruction of the block is a branch referring to us
      IRInstruction last = introducer.getInstructions().getLast();
      if (last instanceof IRBranch) {
        IRBranch i = (IRBranch) last;
        int originalEndPoints = i.getEndPoints();
        boolean modified = false;

        if (i.getThen().getLabel().equals(block.getLabel())) {
          i.getThen().modifyEndPoints(endPoints);
          modified = true;
        } else if (i.getOtherwise().getLabel().equals(block.getLabel())) {
          i.getOtherwise().modifyEndPoints(endPoints);
          modified = true;
        }

        if (modified) {
          // If the end points were modified, we need to propagate the change
          endPoints = i.getEndPoints() - originalEndPoints;
        }
      } else if (last instanceof IRPopType) {
        IRPopType i = (IRPopType) last;
        int originalEndPoints = i.getEndPoints();
        boolean modified = false;

        if (i.getPositive().isPresent()
            && i.getPositive().get().getLabel().equals(block.getLabel())) {
          i.getPositive().get().modifyEndPoints(endPoints);
          modified = true;
        } else if (i.getNegative().isPresent()
            && i.getNegative().get().getLabel().equals(block.getLabel())) {
          i.getNegative().get().modifyEndPoints(endPoints);
          modified = true;
        }

        if (modified) {
          // If the end points were modified, we need to propagate the change
          endPoints = i.getEndPoints() - originalEndPoints;
        }
      } else if (last instanceof IRPopTag) {
        IRPopTag i = (IRPopTag) last;
        int originalEndPoints = i.getEndPoints();
        boolean modified = false;

        for (IRPopTag.Case thisCase : i.getCases().values()) {
          if (thisCase.getLabel().equals(block.getLabel())) {
            thisCase.modifyEndPoints(endPoints);
            modified = true;
            break;
          }
        }

        if (modified) {
          // If the end points were modified, we need to propagate the change
          endPoints = i.getEndPoints() - originalEndPoints;
        }
      }

      // We continue going up the chain of blocks
      modifyEndPoints(ir, introducer, endPoints, visited);
    }
  }

  public void optimizeKnownSlots(IRProgram ir) {
    for (Map.Entry<String, IRProcess> e : ir.getProcesses().entrySet()) {
      if (!processFlows.containsKey(e.getKey())) {
        throw new IllegalStateException(
            "Analysis must be enabled to perform known slots optimization");
      }
      optimizeKnownSlots(e.getValue(), processFlows.get(e.getKey()));
    }
  }

  private void optimizeKnownSlots(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    // Until no changes are made, repeat:
    //
    // - For linear records:
    //    1. Search for IRPopSession which popped a known slot
    //    2. Find respective IRPushSession
    //       - The analyzer can extract this information easily
    //    3. Rename popped record to the pushed record
    //    4. Remove the respective part of the type
    //       - Unsure on how to do this
    //       - Maybe a 'remove nth slot from type' operation
    //    5. Remove both instructions
    //
    // - For exponential records:
    //    1. Search for IRPopExponential which popped a known slot
    //    2. Find respective IRPushExponential or IRPushExpression
    //    3. Rename popped exponential to the pushed exponential
    //    4. Remove the respective part of the type
    //    5. Remove both instructions

    // Will store, for a given push instruction, the instructions which pop their data
    Map<IRFlowLocation, IRFlowLocation> candidatePops = new HashMap<>();

    // We search for any pop instruction which popped a slot with a known pusher
    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      if (!flows.containsKey(block)) {
        continue; // No flow information for this block
      }
      IRFlow flow = flows.get(block);

      for (int i = 0; i < block.getInstructions().size(); ++i) {
        IRInstruction instruction = block.getInstructions().get(i);
        if (!(instruction instanceof IRPop)) {
          continue; // Not what we're looking for
        }
        IRPop pop = (IRPop) instruction;
        if (pop instanceof IRPopSession && ((IRPopSession) pop).getValueRequisites().canBeValue()) {
          continue; // This code currently doesn't handle value session pushes and pops.
        }
        if (pop instanceof IRPopUnfold) {
          continue; // We're not really popping data with these instructions
        }

        IRFlowRecord recordState = flow.getStates().get(i).getBoundRecord(pop.getRecord());
        if (recordState.peek().isEmpty() || recordState.peek().get().getPusher().isEmpty()) {
          continue; // Unknown slot, we can't do anything
        }
        IRFlowLocation pushLoc = recordState.peek().get().getPusher().get();

        candidatePops.put(pushLoc, flow.getLocation(i));
      }
    }

    for (IRFlowLocation pushLoc : candidatePops.keySet()) {
      IRFlowLocation popLoc = candidatePops.get(pushLoc);
      IRPush push = (IRPush) pushLoc.getInstruction();
      IRPop pop = (IRPop) popLoc.getInstruction();

      IRFlowRecord record = pushLoc.getNextState().getBoundRecord(push.getRecord());
      if (record.getNextSlotIndex().isEmpty()) {
        continue; // We need to know which part of the type we're removing
      }
      int slotIndex = record.getNextSlotIndex().get();

      // We must go through each location and ensure that there's a single execution
      // path from the push to the pop.
      if (!pushLoc.hasSinglePathUntil(popLoc)) {
        continue; // We could optimize this across many branches but it is currently unimplemented
      }

      // Depending on the type of pop instruction, we'll try merging their argument data,
      // so that the push/pop pair becomes unnecessary
      boolean removePush = true;
      if (pop instanceof IRPopExponential) {
        int popped = ((IRPopExponential) pop).getArgExponential();

        if (push instanceof IRPushExponential) {
          int pushed = ((IRPushExponential) push).getExponential();

          // We need to remove any IRDetachExponential instructions found for the pushed exponential
          pushLoc.forEachAfter(
              loc -> {
                if (loc.getInstruction() instanceof IRDetachExponential) {
                  IRDetachExponential detach = (IRDetachExponential) loc.getInstruction();
                  if (detach.getExponential() == pushed) {
                    loc.removeInstruction();
                    return false;
                  }
                }
                return true;
              });

          for (IRBlock block : ir.getBlocksIncludingEntry()) {
            for (IRInstruction instr : block.getInstructions()) {
              instr.renameExponentials(r -> r == popped ? pushed : r);
            }
          }
        } else if (push instanceof IRPushExpression) {
          removePush = false;
          IRPushExpression pushExpr = (IRPushExpression) push;
          pushLoc.replaceInstruction(
              new IRNewExponentialExpression(popped, pushExpr.getExpression()));
        } else if (push instanceof IRScan) {
          removePush = false;
          IRScan scan = (IRScan) push;
          pushLoc.replaceInstruction(new IRNewExponentialScan(popped, scan.getType()));
        } else {
          continue; // Unsupported exponential push
        }
      } else if (pop instanceof IRPopSession) {
        int pushed = ((IRPushSession) push).getArgRecord();
        int popped = ((IRPopSession) pop).getArgRecord();
        for (IRBlock block : ir.getBlocksIncludingEntry()) {
          for (IRInstruction instr : block.getInstructions()) {
            instr.renameRecords(r -> r == popped ? pushed : r);
          }
        }
      } else if (pop instanceof IRPopClose) {
        // We don't really need to do anything other than removing the instructions
      } else if (pop instanceof IRPopTag) {
        // For the tags, the jump has already been optimized away by the known jump optimization
        if (!((IRPopTag) pop).getCases().isEmpty()) {
          continue; // Known jump optimization didn't run?
        }
      } else {
        continue; // Unimplemented pop type
      }

      // Now, we simply remove both instructions and modify the type accordingly
      IRType oldType = ir.getRecordType(push.getRecord());
      IRType newType = TypeModifier.removeNth(pushLoc.getPreviousState(), oldType, slotIndex);

      if (newType instanceof IRCloseT) {
        // We might be able to remove the session entirely
        AtomicReference<Optional<IRFlowLocation>> newLoc = new AtomicReference<>(Optional.empty());
        AtomicReference<Optional<IRFlowLocation>> freeLoc = new AtomicReference<>(Optional.empty());

        pushLoc.forEachBefore(
            loc -> {
              if (loc.getInstruction() instanceof IRNewSession) {
                if (((IRNewSession) loc.getInstruction()).getRecord() == push.getRecord()) {
                  newLoc.set(Optional.of(loc));
                  return false;
                }
              } else if (loc.getInstruction().usesRecord(push.getRecord())) {
                return false;
              }
              return true;
            });

        popLoc.forEachAfter(
            loc -> {
              if (loc.getInstruction() instanceof IRFreeSession) {
                if (((IRFreeSession) loc.getInstruction()).getRecord() == push.getRecord()) {
                  freeLoc.set(Optional.of(loc));
                  return false;
                }
              } else if (loc.getInstruction().usesRecord(push.getRecord())) {
                return false;
              }
              return true;
            });

        // If the record is still used between the push and the pop, we can't delete it
        pushLoc.forEachAfter(
            loc -> {
              if (loc != popLoc && loc.getInstruction().usesRecord(push.getRecord())) {
                freeLoc.set(Optional.empty());
                newLoc.set(Optional.empty());
              }
              return loc != popLoc;
            });

        if (newLoc.get().isPresent() && freeLoc.get().isPresent()) {
          newLoc.get().get().removeInstruction();
          freeLoc.get().get().removeInstruction();
        }
      }

      if (removePush) {
        pushLoc.removeInstruction();
      }
      popLoc.removeInstruction();

      ir.setRecordType(push.getRecord(), newType);
    }
  }

  public void removeUnusedRecords(IRProgram ir) {
    for (Map.Entry<String, IRProcess> e : ir.getProcesses().entrySet()) {
      removeUnusedRecords(e.getValue());
    }
  }

  private void removeUnusedRecords(IRProcess ir) {
    // First, figure out which records are unused
    Set<Integer> unusedRecords = new HashSet<>();
    for (int i = ir.getRecordArgumentCount(); i < ir.getRecordCount(); ++i) {
      unusedRecords.add(i);
    }

    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      for (IRInstruction instr : block.getInstructions()) {
        unusedRecords.removeIf(r -> instr.usesRecord(r));
      }
    }

    // Then, remove these records from the process
    Map<Integer, Integer> recordRebindings = new HashMap<>();
    for (int delta = 0, i = 0; i < ir.getRecordCount(); ++i) {
      if (unusedRecords.contains(i)) {
        delta += 1;
      } else {
        recordRebindings.put(i, i - delta);
      }
    }
    for (int i = ir.getRecordCount() - 1; i >= 0; --i) {
      if (unusedRecords.contains(i)) {
        ir.removeRecord(i);
      }
    }

    // Finally, rename all records in the instructions
    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      for (IRInstruction instr : block.getInstructions()) {
        instr.renameRecords(recordRebindings::get);
      }
    }
  }

  public void removeUnusedExponentials(IRProgram ir) {
    for (Map.Entry<String, IRProcess> e : ir.getProcesses().entrySet()) {
      removeUnusedExponentials(e.getValue());
    }
  }

  private void removeUnusedExponentials(IRProcess ir) {
    // First, figure out which exponentials are unused
    Set<Integer> unusedExponentials = new HashSet<>();
    for (int i = ir.getExponentialArgumentCount(); i < ir.getExponentialCount(); ++i) {
      unusedExponentials.add(i);
    }

    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      for (IRInstruction instr : block.getInstructions()) {
        unusedExponentials.removeIf(r -> instr.usesExponential(r));
      }
    }

    // Then, remove these exponentials from the process
    Map<Integer, Integer> exponentialRebindings = new HashMap<>();
    for (int delta = 0, i = 0; i < ir.getExponentialCount(); ++i) {
      if (unusedExponentials.contains(i)) {
        delta += 1;
      } else {
        exponentialRebindings.put(i, i - delta);
      }
    }
    for (int i = ir.getExponentialCount() - 1; i >= 0; --i) {
      if (unusedExponentials.contains(i)) {
        ir.removeExponential(i);
      }
    }

    // Finally, rename all records in the instructions
    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      for (IRInstruction instr : block.getInstructions()) {
        instr.renameExponentials(exponentialRebindings::get);
      }
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
        for (IRFlow target : flow.getTargets()) {
          target.getSources().remove(flow);
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
        flipFlow.removeTarget(forwardFlow);
        forwardFlow.removeSource(flipFlow);
      }
    }
  }

  // Searches for blocks of code like:
  //
  //    pushX(1, x)
  //    ...
  //    pushSession(0, 1, value)
  //
  // and turns them into
  //
  //    pushSession(0, 1, value)
  //    pushX(0, x)
  //
  // This change requires modifying the type of the record being pushed, e.g.,
  // if 1 was of type session(Y); X, then the new type will be session(Y); close.
  public void removeUnnecessaryValuePushes(IRProgram ir) {
    ir.forEachProcess(
        (name, proc) -> {
          removeUnnecessaryValuePushes(proc, processFlows.get(name));
        });
  }

  private void removeUnnecessaryValuePushes(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    Map<Integer, List<IRFlowLocation>> candidates = new HashMap<>();

    // We search for any pushSession instruction which is certainly pushing a value
    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      if (!flows.containsKey(block)) {
        continue; // No flow information for this block
      }
      for (int i = 0; i < block.getInstructions().size(); ++i) {
        IRInstruction instruction = block.getInstructions().get(i);
        if (!(instruction instanceof IRPushSession)) {
          continue; // Not what we're looking for
        }
        IRPushSession pushSession = (IRPushSession) instruction;
        if (!pushSession.getValueRequisites().mustBeValue()) {
          continue; // Not necessarily a value push
        }

        // We store the location of this instruction associated with the record being consumed.
        List<IRFlowLocation> locs =
            candidates.computeIfAbsent(pushSession.getArgRecord(), k -> new ArrayList<>());
        locs.add(flows.get(block).getLocation(i));

        // Now we go back in the flow and look for the first push(argRecord, ...) instruction.
        // If there is branching, we need to check all branches.
      }
    }

    for (int candidate : candidates.keySet()) {
      for (IRFlowLocation pushSessionLoc : candidates.get(candidate)) {
        IRPushSession pushSession = (IRPushSession) pushSessionLoc.getInstruction();
        int argRecord = pushSession.getArgRecord();
        AtomicBoolean recordModified = new AtomicBoolean(false);

        Map<Integer, IRFlowLocation> detachedExponentials = new HashMap<>();
        Map<Integer, IRFlowLocation> decRefExponentials = new HashMap<>();

        pushSessionLoc.forEachBefore(
            loc -> {
              IRInstruction instr = loc.getInstruction();

              if (instr instanceof IRDetachExponential) {
                IRDetachExponential detachExponential = (IRDetachExponential) instr;
                detachedExponentials.put(detachExponential.getExponential(), loc);
              }

              if (instr instanceof IRDecRefExponential) {
                IRDecRefExponential decRefExponential = (IRDecRefExponential) instr;
                decRefExponentials.putIfAbsent(decRefExponential.getExponential(), loc);
              }

              if (instr instanceof IRPush) {
                IRPush push = (IRPush) instr;
                if (push.getRecord() == argRecord) {
                  if (recordModified.get() == false) {
                    // If the main record wasn't modified from this push until the pushSession,
                    // then we can avoid moving the push instruction and just change the record
                    // target
                    //
                    // This requires us moving the push session instruction before this instruction
                    pushSessionLoc.moveInstructionBefore(loc);
                    push.setRecord(pushSession.getRecord());
                  } else {
                    // If it was modified, then we have no choice but to move the push instruction
                    // ahead

                    if (push instanceof IRScan) {
                      // We can't move IRScan instructions, as they have side effects
                      return false;
                    }

                    if (push instanceof IRPushExpression) {
                      // These instructions have associated clean up instructions
                      // We could also move those but that is not implemented yet, so leave them
                      return false;
                    }

                    // If we pushed an exponential and later detached it, we must move the detach to
                    // after the new push.
                    // Additionally, if we decremented the reference count of exponential, we must
                    // move it too.
                    if (push instanceof IRPushExponential) {
                      IRPushExponential pushExponential = (IRPushExponential) push;
                      if (detachedExponentials.containsKey(pushExponential.getExponential())) {
                        IRFlowLocation detachLoc =
                            detachedExponentials.get(pushExponential.getExponential());
                        pushSessionLoc.insertInstructionAfter(detachLoc.getInstruction());
                        detachLoc.removeInstruction();
                      }
                      if (decRefExponentials.containsKey(pushExponential.getExponential())) {
                        IRFlowLocation decRefLoc =
                            decRefExponentials.get(pushExponential.getExponential());
                        pushSessionLoc.insertInstructionAfter(decRefLoc.getInstruction());
                        decRefLoc.removeInstruction();
                      }
                    }

                    push.setRecord(pushSession.getRecord());
                    loc.moveInstructionAfter(pushSessionLoc);
                    ir.setRecordType(
                        argRecord, TypeModifier.removeLast(loc.getPreviousState(), ir.getRecordType(argRecord)));
                  }
                  return true;
                }
              }

              if (instr instanceof IRNewSession) {
                IRNewSession newSession = (IRNewSession) instr;
                if (newSession.getRecord() == argRecord) {
                  loc.removeInstruction();
                  pushSessionLoc.removeInstruction();
                  return false;
                }
              }

              if (instr.usesRecord(argRecord)) {
                return false;
              }

              if (instr.usesRecord(pushSession.getRecord())) {
                recordModified.set(true);
              }

              return true; // Continue iterating
            });
      }
    }
  }

  // Searches for blocks of code like:
  //
  //    popSession(0, 1, value)
  //    ...
  //    popX(1, x)
  //
  // and turns them into
  //
  //    popX(0, x)
  //    popSession(0, 1, value)
  //
  // This change requires modifying the type of the record being popped, e.g.,
  // if 1 was of type session(Y); X, then the new type will be X.
  public void removeUnnecessaryValuePops(IRProgram ir) {
    ir.forEachProcess(
        (name, proc) -> {
          removeUnnecessaryValuePops(proc, processFlows.get(name));
        });
  }

  private void removeUnnecessaryValuePops(IRProcess ir, Map<IRBlock, IRFlow> flows) {
    Map<Integer, List<IRFlowLocation>> candidates = new HashMap<>();

    // We search for any popSession instruction which is certainly popping a value
    for (IRBlock block : ir.getBlocksIncludingEntry()) {
      if (!flows.containsKey(block)) {
        continue; // No flow information for this block
      }
      for (int i = 0; i < block.getInstructions().size(); ++i) {
        IRInstruction instruction = block.getInstructions().get(i);
        if (!(instruction instanceof IRPopSession)) {
          continue; // Not what we're looking for
        }
        IRPopSession popSession = (IRPopSession) instruction;
        if (!popSession.getValueRequisites().mustBeValue()) {
          continue; // Not necessarily a value pop
        }

        // We store the location of this instruction associated with the record being produced.
        List<IRFlowLocation> locs =
            candidates.computeIfAbsent(popSession.getArgRecord(), k -> new ArrayList<>());
        locs.add(flows.get(block).getLocation(i));

        // Now we go ahead in the flow and look for the first pop(argRecord, ...) instruction.
        // If there is branching, we need to check all branches.
      }
    }

    for (int candidate : candidates.keySet()) {
      for (IRFlowLocation popSessionLoc : candidates.get(candidate)) {
        IRPopSession popSession = (IRPopSession) popSessionLoc.getInstruction();
        int argRecord = popSession.getArgRecord();

        popSessionLoc.forEachAfter(
            loc -> {
              IRInstruction instr = loc.getInstruction();

              if (instr instanceof IRPopTag) {
                // We can't move this!
                // Although we maybe could move the pop session to all of the branches?
                return false;
              }

              if (instr instanceof IRPop) {
                IRPop pop = (IRPop) instr;
                if (pop.getRecord() == argRecord) {
                  pop.setRecord(popSession.getRecord());
                  loc.moveInstructionBefore(popSessionLoc);
                  ir.setRecordType(
                      argRecord, TypeModifier.removeFirst(loc.getPreviousState(), ir.getRecordType(argRecord)));
                  return true;
                }
              }

              if (instr instanceof IRFreeSession) {
                IRFreeSession freeSession = (IRFreeSession) instr;
                if (freeSession.getRecord() == argRecord) {
                  loc.removeInstruction();
                  popSessionLoc.removeInstruction();
                  return false;
                }
              }

              if (instr.usesRecord(argRecord)) {
                return false;
              }

              return true; // Continue iterating
            });
      }
    }
  }

  public void inlineProcesses(IRProgram ir, int maxComplexity) {
    Map<String, Integer> evaluated = new HashMap<>();
    Set<String> visited = new HashSet<>();
    ir.forEachProcess((name, proc) -> inlineProcesses(ir, maxComplexity, name, evaluated, visited));
  }

  private void inlineProcesses(
      IRProgram ir,
      int maxComplexity,
      String name,
      Map<String, Integer> evaluated,
      Set<String> visited) {
    if (!visited.add(name)) {
      return;
    }
    IRProcess proc = ir.getProcesses().get(name);

    // Search for process calls which we can inline
    for (int i = 0; i < proc.getBlocksIncludingEntry().size(); ++i) {
      IRBlock block = proc.getBlocksIncludingEntry().get(i);
      IRInstruction instr = block.getInstructions().getLast();
      if (!(instr instanceof IRCallProcess)) {
        continue; // Not a process call
      }

      // First visit the process we'll inline
      IRCallProcess call = (IRCallProcess) instr;
      String callName = call.getProcessName();
      inlineProcesses(ir, maxComplexity, callName, evaluated, visited);
      IRProcess callProc = ir.getProcesses().get(callName);
      if (!callProc.isInlineable()) {
        continue;
      }

      int complexity = evaluated.computeIfAbsent(callName, k -> complexity(callProc));
      if (complexity > maxComplexity) {
        continue; // Skip too complex
      }

      // Remove the call instruction
      block.getInstructions().removeLast();

      // Add new type variables to the current process
      Map<Integer, Integer> typeMap = new HashMap<>();
      for (int j = call.getTypeArguments().size(); j < callProc.getTypeVariableCount(); ++j) {
        typeMap.put(j, proc.addType(callProc.isTypeVariablePositive(j)));
      }

      // Functions which perform variable substitution on the types of the called process.
      BiFunction<Integer, IRValueRequisites, IRValueRequisites> substituteValueReqs =
          (offset, reqs) -> {
            if (reqs.mustBeValue() || !reqs.canBeValue()) {
              return reqs;
            }

            Map<Integer, Boolean> reqPolarities = new HashMap<>();
            List<Integer> reqValues = new ArrayList<>();

            for (int t : reqs.getRequiredTypePolarities().keySet()) {
              boolean p = reqs.getRequiredTypePolarities().get(t);
              if (t < offset) {
                reqPolarities.put(t, p);
                continue;
              }
              t -= offset;

              boolean found = false;
              for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
                if (arg.getTargetType() == t) {
                  if (arg.getSourceTypePolarity() == p) {
                    found = true;
                    break;
                  } else {
                    return IRValueRequisites.notValue();
                  }
                }
              }
              if (!found) {
                if (!typeMap.containsKey(t)) {
                  throw new IllegalStateException(
                      "Type " + t + " not found in type map for process call " + callName);
                }
                reqPolarities.put(typeMap.get(t) + offset, p);
              }
            }

            for (int t : reqs.getTypesWhichMustBeValues()) {
              if (t < offset) {
                reqValues.add(t);
                continue;
              }
              t -= offset;

              boolean found = false;
              for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
                if (arg.getTargetType() == t) {
                  found = true;
                  if (arg.getSourceTypeValueRequisites().mustBeValue()) {
                    break;
                  } else if (!arg.getSourceTypeValueRequisites().canBeValue()) {
                    return IRValueRequisites.notValue();
                  } else {
                    for (Map.Entry<Integer, Boolean> e :
                        arg.getSourceTypeValueRequisites().getRequiredTypePolarities().entrySet()) {
                      reqPolarities.put(e.getKey() + offset, e.getValue());
                    }
                    for (int t2 : arg.getSourceTypeValueRequisites().getTypesWhichMustBeValues()) {
                      reqValues.add(t2 + offset);
                    }
                    break;
                  }
                }
              }
              if (!found) {
                reqValues.add(typeMap.get(t) + offset);
              }
            }

            return IRValueRequisites.value(reqPolarities, reqValues);
          };
      Function<IRType, IRType> substituteTypeVars =
          type -> {
            IRType result = type.substituteReqs(0, substituteValueReqs);
            for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
              result =
                  result.substituteVar(
                      arg.getTargetType(),
                      0,
                      (offset, var) -> {
                        if (var.hasPrecedingFlip(arg.getSourceTypePolarity())) {
                          return new IRFlipT(arg.getSourceType());
                        } else {
                          return arg.getSourceType();
                        }
                      });
            }
            for (int original : typeMap.keySet()) {
              result =
                  result.substituteVar(
                      original,
                      0,
                      (offset, var) -> {
                        return new IRVarT(typeMap.get(original) + offset, var.getFlipPolarity());
                      });
            }
            return result;
          };

      // Start by adding new records, exponentials and types to the current process
      Map<Integer, Integer> recordMap = new HashMap<>();
      for (IRCallProcess.LinearArgument arg : call.getLinearArguments()) {
        recordMap.put(arg.getTargetRecord(), arg.getSourceRecord());
      }
      for (int j = callProc.getRecordArgumentCount(); j < callProc.getRecordCount(); ++j) {
        recordMap.put(j, proc.addRecord(substituteTypeVars.apply(callProc.getRecordType(j))));
      }
      Map<Integer, Integer> exponentialMap = new HashMap<>();
      for (IRCallProcess.ExponentialArgument arg : call.getExponentialArguments()) {
        exponentialMap.put(arg.getTargetExponential(), arg.getSourceExponential());
      }
      for (int j = callProc.getExponentialArgumentCount();
          j < callProc.getExponentialCount();
          ++j) {
        exponentialMap.put(
            j, proc.addExponential(substituteTypeVars.apply(callProc.getExponentialType(j))));
      }

      // Create a mapping from labels in the inlined process to labels in the current process
      Map<String, String> labelMap = new HashMap<>();
      for (IRBlock callBlock : callProc.getBlocks()) {
        labelMap.put(
            callBlock.getLabel(),
            proc.addBlock(call.getProcessName() + "_" + callBlock.getLabel()).getLabel());
      }

      // Add instructions from the entry block into this block
      for (IRInstruction callInstr : callProc.getEntry().getInstructions()) {
        IRInstruction newInstr = callInstr.clone();
        newInstr.renameRecords(recordMap::get);
        newInstr.renameExponentials(exponentialMap::get);
        newInstr.renameLabels(labelMap::get);
        newInstr.substituteTypes(substituteTypeVars, reqs -> substituteValueReqs.apply(0, reqs));
        block.add(newInstr);
      }

      // Add all blocks of the process to the current process
      for (IRBlock callBlock : callProc.getBlocks()) {
        IRBlock newBlock = proc.getBlock(labelMap.get(callBlock.getLabel()));
        for (IRInstruction callInstr : callBlock.getInstructions()) {
          IRInstruction newInstr = callInstr.clone();
          newInstr.renameRecords(recordMap::get);
          newInstr.renameExponentials(exponentialMap::get);
          newInstr.renameLabels(labelMap::get);
          newInstr.substituteTypes(substituteTypeVars, reqs -> substituteValueReqs.apply(0, reqs));
          newBlock.add(newInstr);
        }
      }

      // Modify the end point count of the current block
      modifyEndPoints(proc, block, callProc.getEndPoints() - 1);
    }
  }

  public void removeUnusedProcesses(IRProgram ir, String entryProcess) {
    // BFS to find all processes which are used by the entry process
    Set<String> used = new HashSet<>();
    Queue<String> queue = new LinkedList<>();
    used.add(entryProcess);
    queue.add(entryProcess);

    while (!queue.isEmpty()) {
      String procName = queue.poll();
      IRProcess proc = ir.getProcesses().get(procName);

      for (IRBlock block : proc.getBlocksIncludingEntry()) {
        for (IRInstruction instr : block.getInstructions()) {
          if (instr instanceof IRCallProcess) {
            IRCallProcess call = (IRCallProcess) instr;
            if (!used.contains(call.getProcessName())) {
              used.add(call.getProcessName());
              queue.add(call.getProcessName());
            }
          }
        }
      }
    }

    // Now, we remove all processes which are not used
    ir.getProcesses().entrySet().removeIf(e -> !used.contains(e.getKey()));
  }

  public static class TypeModifier extends IRTypeVisitor {
    private static enum Modification {
      REMOVE_NTH,
      REMOVE_LAST,
    }

    private IRFlowState state;
    private IRType result;
    private Modification modification;
    private int index;

    public static IRType removeFirst(IRFlowState state, IRType type) {
      return removeNth(state, type, 0);
    }

    public static IRType removeNth(IRFlowState state, IRType type, int index) {
      TypeModifier modifier = new TypeModifier(state, Modification.REMOVE_NTH, index);
      return modifier.recurse(type, 0);
    }

    public static IRType removeLast(IRFlowState state, IRType type) {
      TypeModifier modifier = new TypeModifier(state, Modification.REMOVE_LAST, 0);
      return modifier.recurse(type, 0);
    }

    public TypeModifier(IRFlowState state, Modification modification, int index) {
      this.state = state;
      this.modification = modification;
      this.index = index;
    }

    private IRType recurse(IRType type, int offset) {
      TypeModifier m = new TypeModifier(state, modification, index - offset);
      type.accept(m);
      return m.result;
    }

    @Override
    public void visit(IRType type) {
      throw new UnsupportedOperationException(
          "TypeModifier does not yet support type: " + type.getClass().getSimpleName());
    }

    @Override
    public void visit(IRTypeT type) {
      if (index == 0) {
        result = new IRCloseT();
      } else {
        result = type;
      }
    }

    @Override
    public void visit(IRStringT type) {
      result = new IRCloseT();
    }

    @Override
    public void visit(IRBoolT type) {
      result = new IRCloseT();
    }

    @Override
    public void visit(IRIntT type) {
      result = new IRCloseT();
    }

    @Override
    public void visit(IRExponentialT type) {
      result = new IRCloseT();
    }

    @Override
    public void visit(IRCloseT type) {
      result = new IRCloseT();
    }

    @Override
    public void visit(IRCellT type) {
      result = new IRCloseT();
    }

    @Override
    public void visit(IRSessionT type) {
      Optional<Boolean> isValue = state.isValue(type.getValueRequisites());

      if (isValue.isPresent() && isValue.get() == true) {
        if (modification == Modification.REMOVE_NTH) {
          int argCount = TypeSlotCounter.count(type.getArg());
          if (index < argCount) {
            // Remove from the argument side
            IRType arg = recurse(type.getArg(), 0);
            if (arg instanceof IRCloseT) {
              result = type.getCont();
            } else {
              result = new IRSessionT(arg, type.getCont(), type.getValueRequisites());
            }
          } else {
            // Remove from the continuation side
            IRType cont = recurse(type.getCont(), argCount);
            if (cont instanceof IRCloseT) {
              result = type.getArg();
            } else {
              result = new IRSessionT(type.getArg(), cont, type.getValueRequisites());
            }
          }
        } else if (modification == Modification.REMOVE_LAST) {
          IRType cont = recurse(type.getCont(), 0);
          if (cont instanceof IRCloseT) {
            result = type.getArg();
          } else {
            result = new IRSessionT(type.getArg(), cont, type.getValueRequisites());
          }
        }
      } else if (isValue.isPresent() && isValue.get() == false) {
        if (modification == Modification.REMOVE_NTH) {
          if (index == 0) {
            result = type.getCont();
          } else {
            result =
                new IRSessionT(
                    type.getArg(), recurse(type.getCont(), 1), type.getValueRequisites());
          }
        } else if (modification == Modification.REMOVE_LAST) {
          if (type.getCont() instanceof IRCloseT) {
            result = new IRCloseT();
          } else {
            result =
                new IRSessionT(
                    type.getArg(), recurse(type.getCont(), 0), type.getValueRequisites());
          }
        }
      } else {
        throw new UnsupportedOperationException(
            "Sessions with uncertain value requisites are not supported yet");
      }
    }

    @Override
    public void visit(IRTagT type) {
      List<IRType> choices = new ArrayList<>();
      boolean allClose = true;
      for (IRType choice : type.getChoices()) {
        if (!(choice instanceof IRCloseT)) {
          allClose = false;
        }
        choices.add(recurse(choice, 1));
      }

      if (allClose) {
        result = new IRCloseT();
      } else {
        result = new IRTagT(choices);
      }
    }

    @Override
    public void visit(IRFlipT type) {
      type.getCont().accept(this);
    }
  }

  public static class TypeSlotCounter extends IRTypeVisitor {
    private int result;

    public static int count(IRType type) {
      TypeSlotCounter counter = new TypeSlotCounter();
      type.accept(counter);
      return counter.result;
    }

    @Override
    public void visit(IRType type) {
      result = 1;
    }

    @Override
    public void visit(IRSessionT type) {
      if (type.getValueRequisites().mustBeValue()) {
        result = count(type.getArg());
      } else if (!type.getValueRequisites().canBeValue()) {
        result = 1;
      } else {
        throw new UnsupportedOperationException(
            "Sessions with uncertain value requisites are not supported yet");
      }
      result += count(type.getCont());
    }

    @Override
    public void visit(IRTagT type) {
      result = 1;
      for (IRType choice : type.getChoices()) {
        result = Integer.max(result, 1 + count(choice));
      }
    }

    @Override
    public void visit(IRFlipT type) {
      type.getCont().accept(this);
    }
  }

  // Heuristic for how complex a process is
  private int complexity(IRProcess process) {
    return process.getBlocks().size();
  }
}
