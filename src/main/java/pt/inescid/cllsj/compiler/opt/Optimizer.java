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
import java.util.TreeMap;
import java.util.function.BiFunction;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.anl.AnlFlow;
import pt.inescid.cllsj.compiler.anl.AnlFlowContinuation;
import pt.inescid.cllsj.compiler.anl.AnlFlowState;
import pt.inescid.cllsj.compiler.ir.IRTypeFlagRequisites;
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
      if (flow.getBranches().isEmpty() && !flow.hasDetachesInAllTraces()) {
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
        modifyEndPoints(ir, prev.getBlock(), -1);
      }
      i.removeEndPoint();
    } else if (last instanceof IRFinishSession) {
      IRFinishSession i = (IRFinishSession) last;
      AnlFlowContinuation contBefore =
          prev.getStates()
              .getLast()
              .session(i.getSessionId())
              .cont
              .orElseThrow(
                  () ->
                      new IllegalStateException(
                          "Could not find writer for " + i + " on process " + ir.getId()));
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
    for (AnlFlow detached : next.getDetached().keySet()) {
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
        .forEach(
            block -> {
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
      return IRDataLocation.cell(
          optimizeKnownLocation(location.getCell(), state), location.getOffset());
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

  public void optimizeAlwaysDrop(IRProgram ir) {
    for (IRProcess p : ir.stream().toList()) {
      if (!processFlows.containsKey(p.getId())) {
        throw new IllegalStateException(
            "Analysis must be enabled to perform always drop optimization");
      }
      optimizeAlwaysDrop(p, processFlows.get(p.getId()));
    }
  }

  private void optimizeAlwaysDrop(IRProcess ir, Map<IRBlock, AnlFlow> flows) {
    for (IRBlock block : ir.streamBlocks().toList()) {
      AnlFlow flow = flows.get(block);
      if (flow == null || !flow.guaranteedToRun()) {
        continue; // Either unreachable or not guaranteed to run
      }

      for (int i = 0; i < block.size(); ++i) {
        if (!(block.get(i) instanceof IRDeferDrop)) {
          continue; // Not what we're looking for
        }
        IRDeferDrop defer = (IRDeferDrop) block.get(i);
        ir.markAlwaysDropOnEnd(defer.getDropId());
        flow.getLocations().get(i).removeInstruction();
        i -= 1;
      }
    }
  }

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
      if (!ir.isSessionArgument(id)) {
        unusedSessions.add(id);
      }
    }

    Map<IRBlock, TreeMap<Integer, IRSessionId>> toRemove = new HashMap<>();

    for (IRBlock block : ir.streamBlocks().toList()) {
      for (int i = 0; i < block.size(); ++i) {
        IRInstruction instr = block.get(i);
        final int j = i;
        unusedSessions.removeIf(
            s -> {
              if (!instr.usesSession(s)) {
                return false;
              }

              if (instr instanceof IRInitializeSession) {
                IRInitializeSession init = (IRInitializeSession) instr;
                if (init.getSessionId().equals(s)) {
                  toRemove
                      .computeIfAbsent(block, k -> new TreeMap<>(Comparator.reverseOrder()))
                      .put(j, s);
                  return false; // We can just remove the initialization
                }
              }

              return true;
            });
      }
    }

    // Remove any initialization instructions for the removed sessions
    for (IRBlock b : toRemove.keySet()) {
      for (int j : toRemove.get(b).keySet()) {
        if (unusedSessions.contains(toRemove.get(b).get(j))) {
          b.remove(j);
        }
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
      if (!ir.isAssociatedLocalData(id)) {
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
        instr.replaceLocalData(dataRebindings::get, true);
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

  public void inlineProcesses(IRProgram ir, int blockThreshold) {
    if (blockThreshold <= 0) {
      return;
    }

    Set<IRProcessId> visited = new HashSet<>();
    ir.stream().forEach(p -> inlineProcesses(ir, blockThreshold, p.getId(), visited));
  }

  private void inlineProcesses(
      IRProgram ir, int blockThreshold, IRProcessId procId, Set<IRProcessId> visited) {
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
      inlineProcesses(ir, blockThreshold, callId, visited);
      IRProcess callProc = ir.get(callId);
      if (callProc.isRecursive()) {
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
      BiFunction<IRTypeId, IRTypeFlag, IRTypeFlagRequisites> reqReplacer =
          (id, flag) -> {
            for (IRCallProcess.TypeArgument arg : call.getTypeArguments()) {
              if (arg.getTargetType().equals(id)) {
                return Optional.of(arg.getSourceFlags().get(flag)).orElseThrow();
              }
            }
            throw new IllegalStateException("Unbound type variable: " + id);
          };

      // Start by adding the called processes' local data to the current process
      // We must add move instructions to copy the data from the caller to callee
      Map<IRLocalDataId, IRLocalDataId> internalLocalDataMap = new HashMap<>();
      Map<IRLocalDataId, IRDataLocation> argLocalDataMap = new HashMap<>();
      for (IRLocalDataId id : callProc.localData()) {
        // If the local data is associated with a session which is passed as an argument,
        // we can just reuse the local data from the caller
        if (callProc.isAssociatedLocalData(id)) {
          IRSessionId targetSessionId = callProc.getAssociatedSessionId(id).get();
          Optional<IRSessionId> sourceSessionId =
              call.getSessionArguments().stream()
                  .filter(a -> a.getTargetSessionId().equals(targetSessionId))
                  .map(IRCallProcess.SessionArgument::getSourceSessionId)
                  .findFirst();
          if (sourceSessionId.isPresent() && proc.hasLocalData(sourceSessionId.get())) {
            internalLocalDataMap.put(id, proc.getAssociatedLocalData(sourceSessionId.get()).get());
            continue;
          }
        }

        // Check if this local data comes from an argument
        Optional<IRCallProcess.DataArgument> arg =
            call.getDataArguments().stream()
                .filter(a -> a.getTargetDataId().equals(id))
                .findFirst();
        IRSlotCombinations combinations =
            callProc.getLocalData(id).replaceTypes(slotReplacer, reqReplacer);

        if (arg.isPresent() && arg.get().getSourceLocation().isLocal() && !arg.get().isClone()) {
          // If the argument is directly mapped to a local data,
          // instead of creating a new local data and moving the data,
          // we can just reuse the existing local data
          argLocalDataMap.put(id, arg.get().getSourceLocation());
        } else {
          internalLocalDataMap.put(id, proc.addLocalData(combinations));

          // We need to add move instructions to copy the data from the caller to callee
          if (arg.isPresent()) {
            IRLocalDataId targetId = internalLocalDataMap.get(arg.get().getTargetDataId());
            if (arg.get().isClone()) {
              block.add(
                  new IRCloneSlots(
                      IRDataLocation.local(targetId, IRSlotOffset.ZERO),
                      arg.get().getSourceLocation(),
                      arg.get().getSlots()));
            } else {
              block.add(
                  new IRMoveSlots(
                      IRDataLocation.local(targetId, IRSlotOffset.ZERO),
                      arg.get().getSourceLocation(),
                      arg.get().getSlots()));
            }
          }
        }
      }

      // Add the called processes' sessions to the current process
      Map<IRSessionId, IRSessionId> sessionMap = new HashMap<>();
      Map<IRSessionId, IRSlotOffset> sessionOffsetMap = new HashMap<>();
      for (IRCallProcess.SessionArgument arg : call.getSessionArguments()) {
        sessionMap.put(arg.getTargetSessionId(), arg.getSourceSessionId());
        sessionOffsetMap.put(arg.getTargetSessionId(), arg.getDataOffset());
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
        IRDropId newId;

        IRLocalDataId dataId;
        IRSlotOffset offset =
            drop.getOffset().replaceSlots(t -> t.replaceTypes(slotReplacer, reqReplacer));
        IRSlotTree slots = drop.getSlots().replaceTypes(slotReplacer, reqReplacer);

        if (argLocalDataMap.containsKey(drop.getLocalDataId())) {
          dataId = argLocalDataMap.get(drop.getLocalDataId()).getLocalDataId();
          offset = argLocalDataMap.get(drop.getLocalDataId()).getOffset().advance(offset);
        } else {
          dataId = internalLocalDataMap.get(drop.getLocalDataId());
        }

        newId = proc.addDropOnEnd(dataId, offset, slots, false);
        dropMap.put(oldId, newId);
        if (drop.isAlways()) {
          block.add(new IRDeferDrop(newId));
        }
      }

      // Create a mapping from locations in the inlined process to locations in the current process
      Map<IRCodeLocation, IRCodeLocation> locationMap = new HashMap<>();
      locationMap.put(IRCodeLocation.entry(), block.getLocation());
      for (IRBlock callBlock : callProc.streamBlocks().toList()) {
        if (callBlock.getLocation().equals(IRCodeLocation.entry())) {
          continue;
        }
        locationMap.put(
            callBlock.getLocation(),
            proc.createBlock(call.getProcessId() + "_" + callBlock.getLocation().toString())
                .getLocation());
      }

      // Function for converting an instruction from the inlined process to the current process
      Function<IRInstruction, IRInstruction> convertInstruction =
          callInstr -> {
            IRInstruction newInstr = callInstr.clone();
            newInstr.replaceLocalData(internalLocalDataMap::get, false);
            newInstr.replaceDataLocations(
                loc1 ->
                    loc1.replaceDataLocations(
                        loc -> {
                          if (loc.isRemote() && sessionOffsetMap.containsKey(loc.getSessionId())) {
                            return IRDataLocation.remote(
                                loc.getSessionId(),
                                sessionOffsetMap.get(loc.getSessionId()).advance(loc.getOffset()));
                          } else if (loc.isLocal()
                              && argLocalDataMap.containsKey(loc.getLocalDataId())) {
                            return argLocalDataMap
                                .get(loc.getLocalDataId())
                                .advance(loc.getOffset());
                          } else if (loc.isLocal()) {
                            return IRDataLocation.local(
                                internalLocalDataMap.get(loc.getLocalDataId()), loc.getOffset());
                          } else {
                            return loc;
                          }
                        }));
            newInstr.replaceSessions(sessionMap::get);
            newInstr.replaceCodeLocations(locationMap::get);
            newInstr.replaceDropIds(dropMap::get);
            newInstr.replaceTypes(slotReplacer, reqReplacer);
            return newInstr;
          };

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
    Map<IRTypeId, Map<IRTypeFlag, Boolean>> typeFlags = new HashMap<>();
  }

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
              if (arg.isFromLocation() || arg.getSourceTree().isPolymorphic()) {
                candidate = false;
                break;
              }
              args.typeTrees.put(arg.getTargetType(), arg.getSourceTree());
              for (IRTypeFlag flag : arg.getSourceFlags().keySet()) {
                if (arg.getSourceFlags().get(flag).isUncertain()) {
                  candidate = false;
                  break;
                }
                args.typeFlags
                    .computeIfAbsent(arg.getTargetType(), k -> new HashMap<>())
                    .put(flag, arg.getSourceFlags().get(flag).isGuaranteed());
              }
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
              if (arg.getSourceTree().isPolymorphic()) {
                candidate = false;
                break;
              }
              args.typeTrees.put(arg.getTargetType(), arg.getSourceTree());
              for (IRTypeFlag flag : arg.getSourceFlags().keySet()) {
                if (arg.getSourceFlags().get(flag).isUncertain()) {
                  candidate = false;
                  break;
                }
                args.typeFlags
                    .computeIfAbsent(arg.getTargetType(), k -> new HashMap<>())
                    .put(flag, arg.getSourceFlags().get(flag).isGuaranteed());
              }
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
              id ->
                  Optional.ofNullable(args.typeTrees.get(id))
                      .orElseThrow(
                          () -> {
                            throw new IllegalStateException("Unbound type variable: " + id);
                          }),
              (id, flag) -> {
                return Optional.ofNullable(args.typeFlags.get(id)).orElseThrow().get(flag)
                    ? IRTypeFlagRequisites.guaranteed()
                    : IRTypeFlagRequisites.impossible();
              });
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
      for (IRTypeFlag flag : args.typeFlags.get(key).keySet()) {
        sb.append("_");
        sb.append(flag.toString().charAt(0));
        sb.append(args.typeFlags.get(key).get(flag) ? "t" : "f");
      }
      if (!args.typeTrees.get(key).isLeaf()) {
        sb.append("_");
        sb.append(args.typeTrees.get(key).toString().replaceAll("[ ,;\\[\\]\\(\\){}\\|]", ""));
      }
    }

    return new IRProcessId(sb.toString());
  }
}
