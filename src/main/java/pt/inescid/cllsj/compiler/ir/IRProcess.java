package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRProcess {
  private int recordArgumentCount;
  private int exponentialArgumentCount;
  private List<IRType> recordTypes;
  private List<IRType> exponentialTypes;
  private int endPoints;
  private IRBlock entry;
  private List<IRBlock> blocks;
  private List<Boolean> typeVariablePolarities;

  public IRProcess(
      int recordArgumentCount,
      int exponentialArgumentCount,
      List<IRType> recordTypes,
      List<IRType> exponentialTypes,
      List<Boolean> typeVariablePolarites,
      int endPoints) {
    this.recordArgumentCount = recordArgumentCount;
    this.exponentialArgumentCount = exponentialArgumentCount;
    this.recordTypes = recordTypes;
    this.exponentialTypes = exponentialTypes;
    this.typeVariablePolarities = typeVariablePolarites;
    this.endPoints = endPoints;
    this.entry = new IRBlock(null);
    this.blocks = new ArrayList<>();
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
    return recordTypes.size();
  }

  public IRType getRecordType(int index) {
    return recordTypes.get(index);
  }

  public int getExponentialCount() {
    return exponentialTypes.size();
  }

  public IRType getExponentialType(int index) {
    return exponentialTypes.get(index);
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

  public void subtractEndPoints(int endPoints) {
    this.endPoints -= endPoints;
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
