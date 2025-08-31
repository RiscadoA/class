package pt.inescid.cllsj.compiler.ir;

import java.util.ArrayList;
import java.util.List;

import pt.inescid.cllsj.compiler.ir.instructions_old.IRInstruction;

public class IRBlock {
  private String label;
  private List<IRInstruction> instructions;

  public IRBlock(String label) {
    this.label = label;
    this.instructions = new ArrayList<>();
  }

  public String getLabel() {
    return label;
  }

  public List<IRInstruction> getInstructions() {
    return instructions;
  }

  public void add(IRInstruction instruction) {
    instructions.add(instruction);
  }

  @Override
  public String toString() {
    String result = "";
    if (label != null) {
      result += label + ":";
    }
    for (IRInstruction instruction : instructions) {
      if (!result.isEmpty()) {
        result += "\n";
      }
      result += "    " + instruction.toString();
    }
    return result;
  }
}
