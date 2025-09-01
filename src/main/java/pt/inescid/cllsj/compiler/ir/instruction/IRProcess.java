package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;
import pt.inescid.cllsj.compiler.ir.slot.IRSlotCombinations;

public class IRProcess {
  private IRProcessId id;
  private int endPoints;
  private int sessionCount = 0;
  private int typeCount = 0;
  private List<IRSlotCombinations> localData = new ArrayList<>();
  private List<IRBlock> blocks = new ArrayList<>();

  public IRProcess(IRProcessId id, int endPoints) {
    this.id = id;
    this.endPoints = endPoints;
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

  public IRSessionId addSession(IRSlotCombinations localDataCombinations) {
    if (localData.size() != sessionCount) {
      throw new IllegalStateException("Sessions must be added before independent local data");
    }
    sessionCount++;
    localData.add(localDataCombinations);
    return new IRSessionId(sessionCount - 1);
  }

  public int addType() {
    return typeCount++;
  }

  public int getTypeCount() {
    return typeCount;
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

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();

    b.append(id).append(":\n");
    b.append("    end points: ").append(endPoints).append("\n");
    b.append("    sessions: ").append(sessionCount).append("\n");
    b.append("    types: ").append(typeCount).append("\n");
    for (int i = 0; i < localData.size(); i++) {
      b.append("    data ").append(i).append(": ").append(localData.get(i).toString());
    }

    for (IRBlock block : blocks) {
      if (!block.getLocation().equals(IRCodeLocation.entry())) {
        b.append(block.toString());
      }
    }

    return b.toString();
  }
}
