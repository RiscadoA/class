package pt.inescid.cllsj.compiler.opt;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.anl.AnlFlow;
import pt.inescid.cllsj.compiler.anl.AnlFlowContinuation;
import pt.inescid.cllsj.compiler.anl.AnlFlowState;
import pt.inescid.cllsj.compiler.ir.IRValueRequisites;
import pt.inescid.cllsj.compiler.ir.id.*;
import pt.inescid.cllsj.compiler.ir.instruction.*;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class Optimizer {
  private Map<IRProcessId, Map<IRBlock, AnlFlow>> processFlows = new HashMap<>();

  public void feedAnalysis(Map<IRProcessId, Map<IRBlock, AnlFlow>> processFlows) {
    this.processFlows = processFlows;
  }

  public void optimizeKnownJumps(IRProgram ir) {
    for (IRProcess p : ir.stream().toList()) {
      if (!processFlows.containsKey(p.getId())) {
        throw new IllegalStateException(
            "Analysis must be enabled to perform known jumps optimization");
      }
      optimizeKnownJumps(p, processFlows.get(p.getId()));
    }
  }

  private void optimizeKnownJumps(IRProcess ir, Map<IRBlock, AnlFlow> flows) {
    while (true) {
      Optional<AnlFlow> prev = Optional.empty();
      Optional<AnlFlow> next = Optional.empty();

      for (AnlFlow flow : flows.values()) {
        if (flow.getBranches().size() != 1) {
          continue;
        }
        AnlFlow branch = flow.getBranches().stream().findFirst().get();
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
      ir.removeBlock(next.get().getBlock());
      flows.remove(next.get().getBlock());
    }
  }

  public void optimizeKnownEndPoints(IRProgram ir) {
    ir.stream().forEach(p -> optimizeKnownEndPoints(p, processFlows.get(p.getId())));
  }

  private void optimizeKnownEndPoints(IRProcess ir, Map<IRBlock, AnlFlow> flows) {
    for (AnlFlow flow : flows.values()) {
      // We want to find the blocks which have at least one outgoing flow.
      // Those blocks should never be end points.
      // Thus, we skip any which do not have an outgoing flow - these are real endpoints.
      if (flow.getBranches().isEmpty() && flow.getDetached().isEmpty()) {
        continue;
      }

      IRInstruction instruction = flow.getBlock().last();
      boolean wasEndPoint;
      if (instruction instanceof IRCallProcess) {
        wasEndPoint = ((IRCallProcess) instruction).isEndPoint();
        ((IRCallProcess) instruction).removeEndPoint();
      } else if (instruction instanceof IRForwardSessions) {
        wasEndPoint = ((IRForwardSessions) instruction).isEndPoint();
        ((IRForwardSessions) instruction).removeEndPoint();
      } else if (instruction instanceof IRFinishSession) {
        wasEndPoint = ((IRFinishSession) instruction).isEndPoint();
        ((IRFinishSession) instruction).removeEndPoint();
      } else if (instruction instanceof IRPopTask) {
        wasEndPoint = ((IRPopTask) instruction).isEndPoint();
        ((IRPopTask) instruction).removeEndPoint();
      } else {
        continue;
      }

      if (wasEndPoint) {
        modifyEndPoints(ir, flow.getBlock(), -1);
      }
    }
  }

  private void concatFlows(IRProcess ir, AnlFlow prev, AnlFlow next) {
    // Get the last instruction on the current block
    IRInstruction last = prev.getBlock().last();
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
    } else if (last instanceof IRContinueSession) {
      // We need to look for the last time this session's continuation was set and
      // change it to the label stored in this continue.
      IRContinueSession i = (IRContinueSession) last;
      IRSessionId id = i.getSessionId();
      Optional<IRSessionId> remote = prev.getStates().getLast().session(id).remote;
      AnlFlowContinuation contBefore = prev.getStates().getLast().session(id).cont.get();
      AnlFlowContinuation contAfter =
          next.getStates().getFirst().session(remote.orElseThrow()).cont.get();

      contBefore.replaceWritten(Optional.of(i.getContinuation()));
      contAfter.setOverride(contBefore);
      removeLast.run();
    } else if (last instanceof IRBranch) {
      IRBranch i = (IRBranch) last;
      int unusedEndPoints = i.getMaxEndPoints();
      for (IRBranch.Case c : i.getCases()) {
        if (c.getLocation().equals(next.getBlock().getLocation())) {
          unusedEndPoints -= c.getEndPoints();
          break;
        }
      }
      modifyEndPoints(ir, prev.getBlock(), -unusedEndPoints);
      removeLast.run();
    } else if (last instanceof IRForwardSessions) {
      // We keep the forward but avoid the jump to the continuation.
      IRForwardSessions i = (IRForwardSessions) last;
      i.removeJump();

      // We need to remove the label from the previous continuation writer, as it won't be used
      // anymore
      AnlFlowContinuation contBefore = prev.getStates().getLast().session(i.getPosId()).cont.get();
      contBefore.replaceWritten(Optional.empty());

      // We're removing a single end point of the process, if we didn't end up creating a new finish
      // instruction
      if (i.isEndPoint() && !(contBefore.getWriter().getInstruction() instanceof IRFinishSession)) {
        i.removeEndPoint();
        modifyEndPoints(ir, prev.getBlock(), -1);
      }
    } else if (last instanceof IRFinishSession) {
      IRFinishSession i = (IRFinishSession) last;
      AnlFlowContinuation contBefore =
          prev.getStates().getLast().session(i.getSessionId()).cont.get();
      contBefore.replaceWritten(Optional.empty());

      // We're removing a single end point of the process
      if (i.isEndPoint()) {
        modifyEndPoints(ir, prev.getBlock(), -1);
      }

      removeLast.run();
    } else {
      throw new UnsupportedOperationException(
          "Unexpected block ending instruction type: " + last.getClass().getSimpleName());
    }

    // Add the next block's instructions to the current block
    for (int i = 0; i < next.getBlock().size(); ++i) {
      prev.addInstruction(next.getLocation(i));
    }
    prev.getStates().addAll(next.getStates());

    // Link with the outgoing edges of the next flow
    prev.removeTarget(next);
    for (AnlFlow branch : next.getBranches()) {
      branch.removeSource(next);
      branch.addSource(prev);
      prev.addBranch(branch);
    }
    for (AnlFlow detached : next.getDetached()) {
      prev.addDetached(detached);
    }
    for (AnlFlow target : next.getTargets()) {
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
    if (block.getLocation().equals(IRCodeLocation.entry())) {
      ir.setEndPoints(ir.getEndPoints() + endPoints);
      return;
    }

    // Find blocks which reference this block
    for (IRBlock introducer : ir.streamBlocks().toList()) {
      boolean referenced = false;
      for (IRInstruction instr : introducer.stream().toList()) {
        if (instr.usesCodeLocation(block.getLocation())) {
          referenced = true;
          break;
        }
      }
      if (!referenced) {
        continue;
      }

      // We found one! Now we check if the last instruction of the block is a branch referring to us
      IRInstruction last = introducer.last();
      if (last instanceof IRBranch) {
        IRBranch i = (IRBranch) last;
        int originalEndPoints = i.getMaxEndPoints();
        boolean modified = false;

        for (IRBranch.Case thisCase : i.getCases()) {
          if (thisCase.getLocation().equals(block.getLocation())) {
            thisCase.setEndPoints(thisCase.getEndPoints() + endPoints);
            modified = true;
            break;
          }
        }

        if (modified) {
          // If the end points were modified, we need to propagate the change
          endPoints = i.getMaxEndPoints() - originalEndPoints;
        }
      }

      // We continue going up the chain of blocks
      modifyEndPoints(ir, introducer, endPoints, visited);
    }
  }

  // Replaces accesses to known locations with direct accesses.
  // E.g. write(s3...) to write(d2...) if s3 is known to point to d2.
  public void optimizeKnownLocations(IRProgram ir) {
    for (IRProcess p : ir.stream().toList()) {
      if (!processFlows.containsKey(p.getId())) {
        throw new IllegalStateException(
            "Analysis must be enabled to perform known locations optimization");
      }
      optimizeKnownLocations(p, processFlows.get(p.getId()));
    }
  }

  private void optimizeKnownLocations(IRProcess ir, Map<IRBlock, AnlFlow> flows) {
    ir.streamBlocks()
      .forEach(block -> {
        AnlFlow flow = flows.get(block);
        if (flow == null) {
          return; // Unreachable block
        }

        for (int i = 0; i < block.size(); ++i) {
          AnlFlowState state = flow.getStates().get(i);
          block.get(i).replaceDataLocations(loc -> optimizeKnownLocation(loc, state));
        }
      });
  }

  private IRDataLocation optimizeKnownLocation(IRDataLocation location, AnlFlowState state) {
    if (location.isLocal()) {
      return location;
    } else if (location.isCell()) {
      return IRDataLocation.cell(optimizeKnownLocation(location.getCell(), state), location.getOffset());
    } else if (location.isRemote()) {
      Optional<IRDataLocation> known = state.session(location.getSessionId()).data;
      if (known.isPresent()) {
        return known.get().advance(location.getOffset());
      } else {
        return location;
      }
    } else {
      throw new UnsupportedOperationException("Unknown data location type");
    }
  }

  // public void optimizeKnownSlots(IRProgram ir) {
  //   for (IRProcess p : ir.stream().toList()) {
  //     if (!processFlows.containsKey(p.getId())) {
  //       throw new IllegalStateException(
  //           "Analysis must be enabled to perform known slots optimization");
  //     }
  //     optimizeKnownSlots(p, processFlows.get(p.getId()));
  //   }
  // }

  // private void optimizeKnownSlots(IRProcess ir, Map<IRBlock, AnlFlow> flows) {
  //   // Slots which have already been removed of types.
  //   Map<Integer, Set<Integer>> removedSlots = new HashMap<>();

  //   // Will store, for a given push instruction, the instructions which pop their data
  //   Map<AnlFlowLocation, AnlFlowLocation> candidatePops = new HashMap<>();

  //   // We search for any pop instruction which popped a slot with a known pusher
  //   for (IRBlock block : ir.getBlocksIncludingEntry()) {
  //     if (!flows.containsKey(block)) {
  //       continue; // No flow information for this block
  //     }
  //     AnlFlow flow = flows.get(block);

  //     for (int i = 0; i < block.getInstructions().size(); ++i) {
  //       IRInstruction instruction = block.getInstructions().get(i);
  //       if (!(instruction instanceof IRPop)) {
  //         continue; // Not what we're looking for
  //       }
  //       IRPop pop = (IRPop) instruction;
  //       if (pop instanceof IRPopSession && ((IRPopSession)
  // pop).getValueRequisites().canBeValue()) {
  //         continue; // This code currently doesn't handle value session pushes and pops.
  //       }
  //       if (pop instanceof IRPopUnfold) {
  //         continue; // We're not really popping data with these instructions
  //       }

  //       AnlFlowRecord recordState = flow.getStates().get(i).getBoundRecord(pop.getRecord());
  //       if (recordState.peek().isEmpty() || recordState.peek().get().getPusher().isEmpty()) {
  //         continue; // Unknown slot, we can't do anything
  //       }
  //       AnlFlowLocation pushLoc = recordState.peek().get().getPusher().get();

  //       candidatePops.put(pushLoc, flow.getLocation(i));
  //     }
  //   }

  //   for (AnlFlowLocation pushLoc : candidatePops.keySet()) {
  //     AnlFlowLocation popLoc = candidatePops.get(pushLoc);
  //     IRPush push = (IRPush) pushLoc.getInstruction();
  //     IRPop pop = (IRPop) popLoc.getInstruction();

  //     AnlFlowRecord record = pushLoc.getNextState().getBoundRecord(push.getRecord());
  //     if (record.getNextSlotIndex().isEmpty()) {
  //       continue; // We need to know which part of the type we're removing
  //     }
  //     int slotIndex = record.getNextSlotIndex().get() - 1;
  //     if (removedSlots.containsKey(push.getRecord())) {
  //       // We might need to decrement the slot index if previous slots have already been removed
  //       Set<Integer> slots = removedSlots.get(push.getRecord());
  //       slotIndex -= slots.stream().filter(i -> i < record.getNextSlotIndex().get()).count();
  //     }

  //     // We must go through each location and ensure that there's a single execution
  //     // path from the push to the pop.
  //     if (!pushLoc.hasSinglePathUntil(popLoc)) {
  //       continue; // We could optimize this across many branches but it is currently
  // unimplemented
  //     }

  //     // Depending on the type of pop instruction, we'll try merging their argument data,
  //     // so that the push/pop pair becomes unnecessary
  //     boolean removePush = true;
  //     if (pop instanceof IRPopExponential) {
  //       int popped = ((IRPopExponential) pop).getArgExponential();

  //       if (push instanceof IRPushExponential) {
  //         int pushed = ((IRPushExponential) push).getExponential();

  //         // We need to remove any IRDetachExponential instructions found for the pushed
  // exponential
  //         pushLoc.forEachAfter(
  //             loc -> {
  //               if (loc.getInstruction() instanceof IRDetachExponential) {
  //                 IRDetachExponential detach = (IRDetachExponential) loc.getInstruction();
  //                 if (detach.getExponential() == pushed) {
  //                   loc.removeInstruction();
  //                   return false;
  //                 }
  //               }
  //               return true;
  //             });

  //         for (IRBlock block : ir.getBlocksIncludingEntry()) {
  //           for (IRInstruction instr : block.getInstructions()) {
  //             instr.renameExponentials(r -> r == popped ? pushed : r);
  //           }
  //         }
  //       } else if (push instanceof IRPushExpression) {
  //         removePush = false;
  //         IRPushExpression pushExpr = (IRPushExpression) push;
  //         pushLoc.replaceInstruction(
  //             new IRNewExponentialExpression(popped, pushExpr.getExpression()));
  //       } else if (push instanceof IRScan) {
  //         removePush = false;
  //         IRScan scan = (IRScan) push;
  //         pushLoc.replaceInstruction(new IRNewExponentialScan(popped, scan.getType()));
  //       } else {
  //         continue; // Unsupported exponential push
  //       }
  //     } else if (pop instanceof IRPopSession) {
  //       int pushed = ((IRPushSession) push).getArgRecord();
  //       int popped = ((IRPopSession) pop).getArgRecord();
  //       for (IRBlock block : ir.getBlocksIncludingEntry()) {
  //         for (IRInstruction instr : block.getInstructions()) {
  //           instr.renameRecords(r -> r == popped ? pushed : r);
  //         }
  //       }
  //     } else if (pop instanceof IRPopClose) {
  //       // We don't really need to do anything other than removing the instructions
  //     } else if (pop instanceof IRPopTag) {
  //       // For the tags, the jump has already been optimized away by the known jump optimization
  //       if (!((IRPopTag) pop).getCases().isEmpty()) {
  //         continue; // Known jump optimization didn't run?
  //       }
  //     } else {
  //       continue; // Unimplemented pop type
  //     }

  //     // Now, we simply remove both instructions and modify the type accordingly
  //     IRType oldType = ir.getRecordType(push.getRecord());
  //     IRType newType = TypeModifier.removeNth(pushLoc.getPreviousState(), oldType, slotIndex);

  //     if (newType instanceof IRCloseT) {
  //       // We might be able to remove the session entirely
  //       AtomicReference<Optional<AnlFlowLocation>> newLoc = new
  // AtomicReference<>(Optional.empty());
  //       AtomicReference<Optional<AnlFlowLocation>> freeLoc =
  //           new AtomicReference<>(Optional.empty());

  //       pushLoc.forEachBefore(
  //           loc -> {
  //             if (loc.getInstruction() instanceof IRNewSession) {
  //               if (((IRNewSession) loc.getInstruction()).getRecord() == push.getRecord()) {
  //                 newLoc.set(Optional.of(loc));
  //                 return false;
  //               }
  //             } else if (loc.getInstruction().usesRecord(push.getRecord())) {
  //               return false;
  //             }
  //             return true;
  //           });

  //       popLoc.forEachAfter(
  //           loc -> {
  //             if (loc.getInstruction() instanceof IRFreeSession) {
  //               if (((IRFreeSession) loc.getInstruction()).getRecord() == push.getRecord()) {
  //                 freeLoc.set(Optional.of(loc));
  //                 return false;
  //               }
  //             } else if (loc.getInstruction().usesRecord(push.getRecord())) {
  //               return false;
  //             }
  //             return true;
  //           });

  //       // If the record is still used between the push and the pop, we can't delete it
  //       pushLoc.forEachAfter(
  //           loc -> {
  //             if (loc != popLoc && loc.getInstruction().usesRecord(push.getRecord())) {
  //               freeLoc.set(Optional.empty());
  //               newLoc.set(Optional.empty());
  //             }
  //             return loc != popLoc;
  //           });

  //       if (newLoc.get().isPresent() && freeLoc.get().isPresent()) {
  //         newLoc.get().get().removeInstruction();
  //         freeLoc.get().get().removeInstruction();
  //       }
  //     }

  //     if (removePush) {
  //       pushLoc.removeInstruction();
  //     }
  //     popLoc.removeInstruction();

  //     ir.setRecordType(push.getRecord(), newType);
  //     removedSlots.computeIfAbsent(push.getRecord(), k -> new HashSet<>()).add(slotIndex);
  //   }
  // }

  public void removeUnusedSessionsAndData(IRProgram ir) {
    for (IRProcess p : ir.stream().toList()) {
      removeUnusedSessions(p);
      removeUnusedData(p);
    }
  }

  private void removeUnusedSessions(IRProcess ir) {
    // First, figure out which sessions are unused
    Set<IRSessionId> unusedSessions = new HashSet<>();
    for (int i = 0; i < ir.getSessionCount(); ++i) {
      IRSessionId id = new IRSessionId(i);
      if (!ir.isArgSession(id)) {
        unusedSessions.add(id);
      }
    }

    for (IRBlock block : ir.streamBlocks().toList()) {
      for (IRInstruction instr : block.stream().toList()) {
        unusedSessions.removeIf(s -> instr.usesSession(s));
      }
    }

    // Then, remove these sessions from the process
    Map<IRSessionId, IRSessionId> sessionRebindings = new HashMap<>();
    for (int delta = 0, i = 0; i < ir.getSessionCount(); ++i) {
      if (unusedSessions.contains(new IRSessionId(i))) {
        delta += 1;
      } else {
        sessionRebindings.put(new IRSessionId(i), new IRSessionId(i - delta));
      }
    }
    for (int i = ir.getSessionCount() - 1; i >= 0; --i) {
      if (unusedSessions.contains(new IRSessionId(i))) {
        ir.removeSession(new IRSessionId(i));
      }
    }

    // Finally, rename all sessions in the instructions
    ir.replaceSessionsOnHeader(sessionRebindings::get);
    for (IRBlock block : ir.streamBlocks().toList()) {
      for (IRInstruction instr : block.stream().toList()) {
        instr.replaceSessions(sessionRebindings::get);
      }
    }
  }

  private void removeUnusedData(IRProcess ir) {
    // First, figure out which local data entries are unused
    Set<IRLocalDataId> unusedData = new HashSet<>();
    for (int i = 0; i < ir.getLocalDataCount(); ++i) {
      IRLocalDataId id = new IRLocalDataId(i);
      if (!ir.isArgData(id)) {
        unusedData.add(id);
      }
    }

    for (IRProcess.DropOnEnd drop : ir.getDropOnEnd()) {
      unusedData.remove(drop.getLocalDataId());
    }

    for (IRBlock block : ir.streamBlocks().toList()) {
      for (IRInstruction instr : block.stream().toList()) {
        unusedData.removeIf(s -> instr.usesLocalData(s));
      }
    }

    // Then, remove these local data entries from the process
    Map<IRLocalDataId, IRLocalDataId> dataRebindings = new HashMap<>();
    for (int delta = 0, i = 0; i < ir.getLocalDataCount(); ++i) {
      if (unusedData.contains(new IRLocalDataId(i))) {
        delta += 1;
      } else {
        dataRebindings.put(new IRLocalDataId(i), new IRLocalDataId(i - delta));
      }
    }
    for (int i = ir.getLocalDataCount() - 1; i >= 0; --i) {
      if (unusedData.contains(new IRLocalDataId(i))) {
        ir.removeLocalData(new IRLocalDataId(i));
      }
    }

    // Finally, rename all local data entries in the instructions
    ir.replaceLocalDataOnHeader(dataRebindings::get);
    for (IRBlock block : ir.streamBlocks().toList()) {
      for (IRInstruction instr : block.stream().toList()) {
        instr.replaceLocalData(dataRebindings::get);
      }
    }
  }

  public void removeUnreachableBlocks(IRProgram ir) {
    for (IRProcess p : ir.stream().toList()) {
      removeUnreachableBlocks(p, processFlows.get(p.getId()));
    }
  }

  private void removeUnreachableBlocks(IRProcess ir, Map<IRBlock, AnlFlow> flows) {
    while (true) {
      Optional<IRBlock> toRemove = Optional.empty();

      for (IRBlock block : ir.streamBlocks().toList()) {
        if (!flows.containsKey(block)
            || (!block.getLocation().equals(IRCodeLocation.entry())
                && flows.get(block).getSources().isEmpty())) {
          toRemove = Optional.of(block);
          break;
        }
      }

      if (toRemove.isEmpty()) {
        break;
      }

      ir.removeBlock(toRemove.get());
      AnlFlow flow = flows.remove(toRemove.get());
      if (flow != null) {
        for (AnlFlow target : flow.getTargets()) {
          target.removeSource(flow);
        }
      }
    }
  }

  public void inlineProcesses(IRProgram ir, int blockThreshold, boolean allowLoops) {
    if (blockThreshold <= 0) {
      return;
    }

    Set<IRProcessId> visited = new HashSet<>();
    ir.stream().forEach(p -> inlineProcesses(ir, blockThreshold, p.getId(), allowLoops, visited));
  }

  private void inlineProcesses(
      IRProgram ir,
      int blockThreshold,
      IRProcessId procId,
      boolean allowLoops,
      Set<IRProcessId> visited) {
    if (!visited.add(procId)) {
      return;
    }
    IRProcess proc = ir.get(procId);

    // Search for process calls which we can inline
    for (int i = 0, end = proc.getBlockCount(); i < end; ++i) {
      IRBlock block = proc.getBlock(i);
      IRInstruction instr = block.last();
      if (!(instr instanceof IRCallProcess)) {
        continue; // Not a process call
      }

      // First visit the process we'll inline
      IRCallProcess call = (IRCallProcess) instr;
      IRProcessId callId = call.getProcessId();
      inlineProcesses(ir, blockThreshold, callId, allowLoops, visited);
      IRProcess callProc = ir.get(callId);
      if (!allowLoops && callProc.isRecursive()) {
        continue;
      }
      if (callId.equals(procId)) {
        continue; // We can't inline a process into itself
      }
      if (callProc.getBlockCount() > blockThreshold) {
        continue; // Skip, too complex
      }
      // Inlined processes must not have type variables dependent on data
      boolean skip = false;
      for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
        if (arg.isFromLocation()) {
          skip = true;
          break;
        }
      }
      if (skip) {
        continue;
      }

      // If the process is recursive, we need to check if we can turn it into a loop
      if (callProc.isRecursive()) {
        // It must have a single end point - otherwise, tail calls are not guaranteed
        if (callProc.getEndPoints() != 1) {
          continue;
        }

        // All recursive calls must be end points, and thus, be tail calls
        boolean valid = true;
        for (IRInstruction innerInstr : callProc.streamInstructions().toList()) {
          if (innerInstr instanceof IRCallProcess) {
            IRCallProcess innerCall = (IRCallProcess) innerInstr;
            if (innerCall.getProcessId().equals(callId) && !innerCall.isEndPoint()) {
              valid = false;
              break;
            }

            // We must also be able to turn the call into a loop
            throw new UnsupportedOperationException("TODO: implement IRCallLoop");
            // if (!IRCallLoop.canGetFromCallProcess(innerCall)) {
            //   valid = false;
            //   break;
            // }
          }
        }
        if (!valid) {
          continue;
        }
      }

      // Remove the call instruction
      block.remove(block.size() - 1);

      // Functions for substituting type variables
      Function<IRTypeId, IRSlotTree> slotReplacer =
          id -> {
            for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
              if (arg.getTargetType().equals(id)) {
                return arg.getSourceTree();
              }
            }
            throw new IllegalStateException("Unbound type variable: " + id);
          };
      Function<IRTypeId, IRValueRequisites> reqReplacer =
          id -> {
            for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
              if (arg.getTargetType().equals(id)) {
                return arg.getSourceIsValue();
              }
            }
            throw new IllegalStateException("Unbound type variable: " + id);
          };

      // Start by adding the called processes' local data to the current process
      // We must add move instructions to copy the data from the caller to callee
      Map<IRLocalDataId, IRLocalDataId> localDataMap = new HashMap<>();
      for (IRLocalDataId id : callProc.localData()) {
        IRSlotCombinations combinations =
            callProc.getLocalData(id).replaceTypes(slotReplacer, reqReplacer);
        localDataMap.put(id, proc.addLocalData(combinations));
      }
      for (IRCallProcess.DataArgument arg : call.getDataArguments()) {
        // We need to add move instructions to copy the data from the caller to callee
        IRLocalDataId targetId = localDataMap.get(arg.getTargetDataId());
        if (arg.isClone()) {
          block.add(
              new IRCloneValue(
                  IRDataLocation.local(targetId, IRSlotOffset.ZERO),
                  arg.getSourceLocation(),
                  arg.getSlots()));
        } else {
          block.add(
              new IRMoveValue(
                  IRDataLocation.local(targetId, IRSlotOffset.ZERO),
                  arg.getSourceLocation(),
                  arg.getSlots()));
        }
      }

      // Add the called processes' sessions to the current process
      // We must add bind instructions to link the sessions from caller to callee
      Map<IRSessionId, IRSessionId> sessionMap = new HashMap<>();
      for (IRCallProcess.SessionArgument arg : call.getSessionArguments()) {
        IRSessionId newId = proc.addSession();
        sessionMap.put(arg.getTargetSessionId(), newId);
        IRLocalDataId dataId = callProc.getArgSessionLocalDataId(arg.getTargetSessionId()).get();
        dataId = localDataMap.get(dataId);
        block.add(
            new IRBindSession(
                newId,
                arg.getSourceSessionId(),
                arg.getDataOffset(),
                IRDataLocation.local(dataId, IRSlotOffset.ZERO)));
      }
      for (IRSessionId id : callProc.sessions()) {
        if (sessionMap.containsKey(id)) {
          continue; // Already handled above
        }
        sessionMap.put(id, proc.addSession());
      }

      // Add the drop on end markers
      Map<IRDropId, IRDropId> dropMap = new HashMap<>();
      for (int old = 0; old < callProc.getDropOnEnd().size(); ++old) {
        IRDropId oldId = new IRDropId(old);
        IRProcess.DropOnEnd drop = callProc.getDropOnEnd(oldId);
        IRDropId newId =
            proc.addDropOnEnd(
                localDataMap.get(drop.getLocalDataId()),
                drop.getOffset().replaceSlots(t -> t.replaceTypes(slotReplacer, reqReplacer)),
                drop.getSlots().replaceTypes(slotReplacer, reqReplacer),
                false);
        dropMap.put(oldId, newId);
        if (drop.isAlways()) {
          block.add(new IRDeferDrop(newId));
        }
      }

      // Identify the block we'll be adding the inlined process' entry instructions
      // If the process is recursive (and thus, a loop), we need to create a new block
      IRBlock entryBlock;
      if (callProc.isRecursive()) {
        entryBlock = proc.createBlock(call.getProcessId().toString());
        throw new UnsupportedOperationException("TODO: implement IRCallLoop");
        // block.add(
        //     IRCallLoop.fromCallProcess(
        //             entryBlock.getLabel(), call, recordMap::get, exponentialMap::get)
        //         .get());
      } else {
        entryBlock = block;
      }

      // Create a mapping from locations in the inlined process to locations in the current process
      Map<IRCodeLocation, IRCodeLocation> locationMap = new HashMap<>();
      for (IRBlock callBlock : callProc.streamBlocks().toList()) {
        locationMap.put(
            callBlock.getLocation(),
            proc.createBlock(call.getProcessId() + "_" + callBlock.getLocation().toString())
                .getLocation());
      }

      // Function for converting an instruction from the inlined process to the current process
      Function<IRInstruction, IRInstruction> convertInstruction =
          callInstr -> {
            IRInstruction newInstr = callInstr.clone();
            newInstr.replaceSessions(sessionMap::get);
            newInstr.replaceLocalData(localDataMap::get);
            newInstr.replaceCodeLocations(locationMap::get);
            newInstr.replaceDropIds(dropMap::get);
            newInstr.replaceTypes(slotReplacer, reqReplacer);

            // If it is a recursive call, turn into a IRCallLoop instruction
            if (callProc.isRecursive() && newInstr instanceof IRCallProcess) {
              IRCallProcess newCall = (IRCallProcess) newInstr;
              if (newCall.getProcessId().equals(callId)) {
                throw new UnsupportedOperationException("TODO: implement IRCallLoop");
                // newInstr =
                //     IRCallLoop.fromCallProcess(
                //             entryBlock.getLabel(), newCall, recordMap::get, exponentialMap::get)
                //         .get();
              }
            }

            // We might need to remove the end point from the instruction
            // if (!call.isEndPoint()) {
            //   if (newInstr instanceof IRCallProcess) {
            //     ((IRCallProcess) newInstr).removeEndPoint();
            //   } else if (newInstr instanceof IRForward) {
            //     ((IRForward) newInstr).removeEndPoint();
            //   } else if (newInstr instanceof IRFlipForward) {
            //     ((IRFlipForward) newInstr).removeEndPoint();
            //   } else if (newInstr instanceof IRReturn) {
            //     ((IRReturn) newInstr).removeEndPoint();
            //   } else if (newInstr instanceof IRNextTask) {
            //     ((IRNextTask) newInstr).removeEndPoint();
            //   }
            // }

            return newInstr;
          };

      // Add instructions from the entry block into the entryBlock
      for (IRInstruction callInstr : callProc.getEntry().stream().toList()) {
        entryBlock.add(convertInstruction.apply(callInstr));
      }

      // Add all blocks of the process to the current process
      for (IRBlock callBlock : callProc.streamBlocks().toList()) {
        IRBlock newBlock = proc.getBlock(locationMap.get(callBlock.getLocation()));
        for (IRInstruction callInstr : callBlock.stream().toList()) {
          newBlock.add(convertInstruction.apply(callInstr));
        }
      }

      // Modify the end point count of the current block
      if (call.isEndPoint()) {
        modifyEndPoints(proc, block, callProc.getEndPoints() - 1);
      } else {
        modifyEndPoints(proc, block, callProc.getEndPoints());
      }
    }
  }

  public void removeUnusedProcesses(IRProgram ir, IRProcessId entryProcess) {
    // BFS to find all processes which are used by the entry process
    Set<IRProcessId> used = new HashSet<>();
    Queue<IRProcessId> queue = new LinkedList<>();
    used.add(entryProcess);
    queue.add(entryProcess);

    while (!queue.isEmpty()) {
      IRProcessId procName = queue.poll();
      IRProcess proc = ir.get(procName);

      for (IRBlock block : proc.streamBlocks().toList()) {
        for (IRInstruction instr : block.stream().toList()) {
          instr.replaceProcesses(
              id -> {
                if (used.add(id)) {
                  queue.add(id);
                }
                return id;
              });
        }
      }
    }

    // Now, we remove all processes which are not used
    ir.removeIf(p -> !used.contains(p.getId()));
  }

  private static class MonomorphizationArgs {
    Map<IRTypeId, IRSlotTree> typeTrees = new HashMap<>();
    Map<IRTypeId, Boolean> typeIsValue = new HashMap<>();
  };

  public void monomorphizeProcesses(IRProgram ir) {
    // While there are process calls / exponential writes with concrete type arguments
    // keep monomorphizing them
    while (true) {
      Map<IRProcessId, Map<IRProcessId, MonomorphizationArgs>> toMonomorphize = new HashMap<>();

      // Search for process calls / exponential writes to monomorphize
      for (IRProcess proc : ir.stream().toList()) {
        for (IRInstruction instr : proc.streamInstructions().toList()) {
          if (instr instanceof IRCallProcess) {
            IRCallProcess call = (IRCallProcess) instr;
            MonomorphizationArgs args = new MonomorphizationArgs();
            boolean candidate = !call.getTypeArguments().isEmpty();
            for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
              if (arg.isFromLocation() || arg.getSourceTree().isPolymorphic() || arg.getSourceIsValue().isUncertain()) {
                candidate = false;
                break;
              }
              args.typeTrees.put(arg.getTargetType(), arg.getSourceTree());
              args.typeIsValue.put(arg.getTargetType(), arg.getSourceIsValue().mustBeValue());
            }
            if (!candidate) {
              // No type arguments or not all concrete, not a valid candidate
              continue;
            }

            IRProcessId newId = monomorphize(call.getProcessId(), args);
            if (!ir.contains(newId)) {
              toMonomorphize
                  .computeIfAbsent(call.getProcessId(), k -> new HashMap<>())
                  .putIfAbsent(newId, args);
            }

            // Modify the call instruction to use the monomorphized process
            call.setProcessId(newId);
            call.removeTypeArguments();
          } else if (instr instanceof IRWriteExponential) {
            IRWriteExponential write = (IRWriteExponential) instr;
            MonomorphizationArgs args = new MonomorphizationArgs();
            boolean candidate = !write.getTypeArguments().isEmpty();
            for (IRWriteExponential.TypeArgument arg : write.getTypeArguments()) {
              if (arg.getSourceTree().isPolymorphic() || arg.getSourceIsValue().isUncertain()) {
                candidate = false;
                break;
              }
              args.typeTrees.put(arg.getTargetType(), arg.getSourceTree());
              args.typeIsValue.put(arg.getTargetType(), arg.getSourceIsValue().mustBeValue());
            }
            if (!candidate) {
              // No type arguments or not all concrete, not a valid candidate
              continue;
            }

            IRProcessId newId = monomorphize(write.getProcessId(), args);
            if (!ir.contains(newId)) {
              toMonomorphize
                  .computeIfAbsent(write.getProcessId(), k -> new HashMap<>())
                  .putIfAbsent(newId, args);
            }

            // Modify the write instruction to use the monomorphized process
            write.setProcessId(newId);
            write.removeTypeArguments();
          }
        }
      }

      if (toMonomorphize.isEmpty()) {
        break;
      }

      // We now have a list of processes to monomorphize
      for (IRProcessId originalId : toMonomorphize.keySet()) {
        for (IRProcessId newId : toMonomorphize.get(originalId).keySet()) {
          // Clone the old process, remove all type variables, and replace types on instructions
          MonomorphizationArgs args = toMonomorphize.get(originalId).get(newId);
          IRProcess originalProc = ir.get(originalId);
          IRProcess newProc = originalProc.clone(newId);
          newProc.removeTypes();
          newProc.replaceTypes(
            id -> args.typeTrees.get(id),
            id -> args.typeIsValue.get(id) ? IRValueRequisites.value() : IRValueRequisites.notValue()
          );
          ir.add(newProc);
        }
      }
    }
  }

  private IRProcessId monomorphize(IRProcessId baseId, MonomorphizationArgs args) {
    StringBuilder sb = new StringBuilder();
    sb.append(baseId.toString());
    sb.append("_monomorphized");
    List<IRTypeId> keys = new ArrayList<>(args.typeTrees.keySet());
    keys.sort(Comparator.comparing(IRTypeId::toString));

    for (IRTypeId key : keys) {
      sb.append("_");
      sb.append(key);
      sb.append("_");
      sb.append(args.typeIsValue.get(key) ? "v" : "n");
      if (!args.typeTrees.get(key).isLeaf()) {
        sb.append("_");
        sb.append(args.typeTrees.get(key).toString().replaceAll("[ ,;\\[\\]\\(\\){}\\|]", ""));
      }
    }

    return new IRProcessId(sb.toString());
  }
}
