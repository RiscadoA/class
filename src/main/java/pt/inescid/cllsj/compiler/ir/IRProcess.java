package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import pt.inescid.cllsj.compiler.ir.instructions_old.IRInstruction;

public class IRProcess {
  private int recordArgumentCount;
  private int exponentialArgumentCount;
  private int recordCount;
  private int exponentialCount;
  private int endPoints;
  private IRBlock entry;
  private List<IRBlock> blocks;
  private List<Boolean> typeVariablePolarities;
  private boolean inlineable;
  private boolean recursive;

  public IRProcess(
      int recordArgumentCount,
      int exponentialArgumentCount,
      int recordCount,
      int exponentialCount,
      List<Boolean> typeVariablePolarities,
      int endPoints,
      boolean inlineable,
      boolean recursive) {
    this.recordArgumentCount = recordArgumentCount;
    this.exponentialArgumentCount = exponentialArgumentCount;
    this.recordCount = recordCount;
    this.exponentialCount = exponentialCount;
    this.typeVariablePolarities = new ArrayList<>(typeVariablePolarities);
    this.endPoints = endPoints;
    this.entry = new IRBlock(null);
    this.blocks = new ArrayList<>();
    this.inlineable = inlineable;
    this.recursive = recursive;
  }

  public boolean hasArguments() {
    return recordArgumentCount > 0 || exponentialArgumentCount > 0;
  }

  public int getRecordArgumentCount() {
    return recordArgumentCount;
  }

  public int getExponentialArgumentCount() {
    return exponentialArgumentCount;
  }

  public int getRecordCount() {
    return recordCount;
  }

  public int addRecord() {
    return recordCount++;
  }

  public void removeRecord() {
    recordCount--;
  }

  public int getExponentialCount() {
    return exponentialCount;
  }

  public int addExponential() {
    return exponentialCount++;
  }

  public void removeExponential() {
    exponentialCount--;
  }

  public int addType(boolean polarity) {
    int index = typeVariablePolarities.size();
    typeVariablePolarities.add(polarity);
    return index;
  }

  public int getTypeVariableCount() {
    return typeVariablePolarities.size();
  }

  public boolean isTypeVariablePositive(int index) {
    return typeVariablePolarities.get(index);
  }

  public int getEndPoints() {
    return endPoints;
  }

  public void setEndPoints(int endPoints) {
    this.endPoints = endPoints;
  }

  public IRBlock getEntry() {
    return entry;
  }

  public IRBlock addBlock(String type) {
    IRBlock block = new IRBlock(type + "_" + blocks.size());
    blocks.add(block);
    return block;
  }

  public List<IRBlock> getBlocks() {
    return blocks;
  }

  public List<IRBlock> getBlocksIncludingEntry() {
    return Stream.concat(Stream.of(entry), blocks.stream()).toList();
  }

  public List<IRInstruction> getInstructions() {
    return getBlocksIncludingEntry().stream().flatMap(b -> b.getInstructions().stream()).toList();
  }

  public IRBlock getBlock(String label) {
    for (IRBlock block : blocks) {
      if (block.getLabel().equals(label)) {
        return block;
      }
    }
    throw new IllegalArgumentException("Block with label " + label + " does not exist");
  }

  public boolean containsBlock(String label) {
    for (IRBlock block : blocks) {
      if (block.getLabel().equals(label)) {
        return true;
      }
    }
    return false;
  }

  public boolean isInlineable() {
    return inlineable;
  }

  public boolean isRecursive() {
    return recursive;
  }

  @Override
  public String toString() {
    String result = entry.toString();
    for (IRBlock block : blocks) {
      if (!result.isEmpty()) {
        result += "\n";
      }
      result += block.toString();
    }
    return result;
  }
}
