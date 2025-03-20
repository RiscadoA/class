package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;

public class IRProcess {
  private boolean hasArguments;
  private int recordCount;
  private int typeCount;
  private IRBlock entry;
  private List<IRBlock> blocks;

  public IRProcess(boolean hasArguments, int recordCount, int typeCount) {
    this.hasArguments = hasArguments;
    this.recordCount = recordCount;
    this.typeCount = typeCount;
    this.entry = new IRBlock(null);
    this.blocks = new ArrayList<>();
  }

  public boolean hasArguments() {
    return hasArguments;
  }

  public int getRecordCount() {
    return recordCount;
  }

  public int getTypeCount() {
    return typeCount;
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
