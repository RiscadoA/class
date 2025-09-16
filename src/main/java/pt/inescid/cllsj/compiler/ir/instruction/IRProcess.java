package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDropId;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.id.IRTypeId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotOffset;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotTree;

public class IRProcess {
  private IRProcessId id;
  private int endPoints;
  private int typeCount = 0;
  private int sessionCount = 0;
  private Map<IRSessionId, IRLocalDataId> argSessionLocalDataId = new HashMap<>();
  private List<DropOnEnd> dropOnEnd = new ArrayList<>();
  private List<IRSlotCombinations> localData = new ArrayList<>();
  private List<IRBlock> blocks = new ArrayList<>();

  // Identifies a memory location to be dropped when the process ends
  public static class DropOnEnd {
    private IRLocalDataId localDataId; // Local data entry id
    private IRSlotOffset offset; // Offset from the start of the local data entry
    private IRSlotTree slots; // Slots to drop
    private boolean always; // If false, we must store data on whether the slot needs to be dropped

    public DropOnEnd(
        IRLocalDataId localDataId, IRSlotOffset offset, IRSlotTree slots, boolean always) {
      this.localDataId = localDataId;
      this.offset = offset;
      this.slots = slots;
      this.always = always;
    }

    public IRLocalDataId getLocalDataId() {
      return localDataId;
    }

    public IRSlotOffset getOffset() {
      return offset;
    }

    public IRSlotTree getSlots() {
      return slots;
    }

    public boolean isAlways() {
      return always;
    }

    @Override
    public String toString() {
      StringBuilder b = new StringBuilder();
      b.append(slots).append(" at ");
      b.append(localDataId);
      b.append(offset);
      if (always) {
        b.append(" (always)");
      }
      return b.toString();
    }
  }

  public IRProcess(IRProcessId id) {
    this.id = id;
    this.endPoints = 1;
    blocks.add(new IRBlock(IRCodeLocation.entry()));
  }

  public IRProcessId getId() {
    return id;
  }

  public int getEndPoints() {
    return endPoints;
  }

  public void setEndPoints(int endPoints) {
    this.endPoints = endPoints;
  }

  public int getSessionCount() {
    return sessionCount;
  }

  public IRSessionId addSession() {
    return new IRSessionId(sessionCount++);
  }

  public IRSessionId addArgSession(IRLocalDataId localDataId) {
    IRSessionId sessionId = new IRSessionId(sessionCount++);
    argSessionLocalDataId.put(sessionId, localDataId);
    return sessionId;
  }

  public IRTypeId addType() {
    return new IRTypeId(typeCount++);
  }

  public int getTypeCount() {
    return typeCount;
  }

  public Optional<IRLocalDataId> getArgSessionLocalDataId(IRSessionId sessionId) {
    return Optional.ofNullable(argSessionLocalDataId.get(sessionId));
  }

  public IRSlotCombinations getLocalData(IRLocalDataId id) {
    return localData.get(id.getIndex());
  }

  public IRLocalDataId addLocalData(IRSlotCombinations combinations) {
    localData.add(combinations);
    return new IRLocalDataId(localData.size() - 1);
  }

  public int getLocalDataCount() {
    return localData.size();
  }

  public Stream<IRSlotCombinations> streamLocalData() {
    return localData.stream();
  }

  public IRDropId addDropOnEnd(
      IRLocalDataId localDataId, IRSlotOffset offset, IRSlotTree slots, boolean always) {
    dropOnEnd.add(new DropOnEnd(localDataId, offset, slots, always));
    return new IRDropId(dropOnEnd.size() - 1);
  }

  public List<DropOnEnd> getDropOnEnd() {
    return dropOnEnd;
  }

  public DropOnEnd getDropOnEnd(IRDropId id) {
    return dropOnEnd.get(id.getIndex());
  }

  public IRBlock getEntry() {
    return blocks.getFirst();
  }

  public IRBlock getBlock(IRCodeLocation location) {
    return blocks.stream().filter(b -> b.getLocation().equals(location)).findFirst().get();
  }

  public Stream<IRBlock> streamBlocks() {
    return blocks.stream();
  }

  public Stream<IRInstruction> getInstructions() {
    return streamBlocks().flatMap(b -> b.stream());
  }

  public IRBlock createBlock(String type) {
    String label = type + "_" + (blocks.size() - 1);
    IRBlock block = new IRBlock(IRCodeLocation.label(label));
    blocks.add(block);
    return block;
  }

  public void removeBlock(IRBlock block) {
    blocks.remove(block);
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();

    b.append(id).append(":\n");
    b.append("    end points: ").append(endPoints).append("\n");
    b.append("    types:");
    if (typeCount == 0) {
      b.append(" none");
    } else {
      for (int i = 0; i < typeCount; i++) {
        b.append(" ").append(new IRTypeId(i));
      }
    }
    b.append("\n");
    b.append("    sessions:");
    for (int i = 0; i < sessionCount; i++) {
      IRSessionId sessionId = new IRSessionId(i);
      b.append(" ").append(sessionId);
    }
    b.append("\n");
    for (int i = 0; i < localData.size(); i++) {
      b.append("    data ")
          .append(new IRLocalDataId(i))
          .append(": ")
          .append(localData.get(i).toString());
      for (Map.Entry<IRSessionId, IRLocalDataId> entry : argSessionLocalDataId.entrySet()) {
        if (entry.getValue().getIndex() == i) {
          b.append(" (arg ").append(entry.getKey().toString()).append(")");
          break;
        }
      }
      b.append("\n");
    }
    for (int i = 0; i < dropOnEnd.size(); ++i) {
      b.append("    drop ").append(new IRDropId(i)).append(": ");
      b.append(dropOnEnd.get(i)).append("\n");
    }

    for (IRBlock block : blocks) {
      b.append(block.toString());
    }

    return b.toString();
  }
}
