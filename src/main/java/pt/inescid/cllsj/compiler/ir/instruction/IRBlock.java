package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;

public class IRBlock {
  private IRCodeLocation location;
  private List<IRInstruction> instructions = new ArrayList<>();

  public IRBlock(IRCodeLocation location) {
    this.location = location;
  }

  public IRCodeLocation getLocation() {
    return location;
  }

  public void add(IRInstruction instr) {
    instructions.add(instr);
  }

  public void add(int index, IRInstruction instr) {
    instructions.add(index, instr);
  }

  public IRInstruction get(int index) {
    return instructions.get(index);
  }

  public IRInstruction last() {
    return instructions.getLast();
  }

  public void set(int index, IRInstruction instr) {
    instructions.set(index, instr);
  }

  public IRInstruction remove(int index) {
    return instructions.remove(index);
  }

  public int size() {
    return instructions.size();
  }

  public Stream<IRInstruction> stream() {
    return instructions.stream();
  }

  public IRBlock clone() {
    IRBlock newBlock = new IRBlock(location);
    newBlock.instructions = new ArrayList<>(instructions.stream()
        .map(instr -> instr.clone())
        .toList());
    return newBlock;
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    b.append(location.toString() + ":\n");
    for (IRInstruction instr : instructions) {
      b.append("    " + instr.toString() + "\n");
    }
    return b.toString();
  }
}
