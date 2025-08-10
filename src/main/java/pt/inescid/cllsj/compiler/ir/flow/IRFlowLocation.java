package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;

public class IRFlowLocation {
  private IRFlow flow;
  private int index;
  private boolean removed = false;

  public IRFlowLocation(IRFlow flow, int index) {
    this.flow = flow;
    this.index = index;
  }

  public static IRFlowLocation initial(int index) {
    return new IRFlowLocation(null, -1 - index);
  }

  public IRFlow getFlow() {
    if (removed) {
      throw new IllegalStateException("This flow location (" + this + ") has been removed");
    }
    return flow;
  }

  public int getIndex() {
    if (removed) {
      throw new IllegalStateException("This flow location (" + this + ") has been removed");
    }
    return index;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (removed) {
      sb.append("removed ");
    }
    if (index < 0) {
      sb.append("initial");
    } else {
      sb.append(Optional.ofNullable(flow.getBlock().getLabel()).orElse("entry"));
      sb.append(":").append(index);
    }
    return sb.toString();
  }

  public void move(IRFlow newFlow, int newIndex) {
    if (removed) {
      throw new IllegalStateException("This flow location (" + this + ") has been removed");
    }

    this.flow = newFlow;
    this.index = newIndex;
  }

  public void markRemoved() {
    this.removed = true;
  }

  public IRInstruction getInstruction() {
    return getFlow().getBlock().getInstructions().get(getIndex());
  }

  public void replaceInstruction(IRInstruction newInstruction) {
    getFlow().getBlock().getInstructions().set(getIndex(), newInstruction);
  }
}
