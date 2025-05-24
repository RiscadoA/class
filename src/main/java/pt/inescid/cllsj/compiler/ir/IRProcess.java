package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;
import pt.inescid.cllsj.compiler.ir.type.IRType;

public class IRProcess {
  private boolean hasArguments;
  private List<IRType> recordTypes;
  private List<IRType> exponentialTypes;
  private int endPoints;
  private IRBlock entry;
  private List<IRBlock> blocks;

  public IRProcess(
      boolean hasArguments,
      List<IRType> recordTypes,
      List<IRType> exponentialTypes,
      int endPoints) {
    this.hasArguments = hasArguments;
    this.recordTypes = recordTypes;
    this.exponentialTypes = exponentialTypes;
    this.endPoints = endPoints;
    this.entry = new IRBlock(null);
    this.blocks = new ArrayList<>();
  }

  public boolean hasArguments() {
    return hasArguments;
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

  public int getEndPoints() {
    return endPoints;
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
