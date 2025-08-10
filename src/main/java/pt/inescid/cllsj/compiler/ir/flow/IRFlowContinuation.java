package pt.inescid.cllsj.compiler.ir.flow;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.instructions.IRFlip;
import pt.inescid.cllsj.compiler.ir.instructions.IRInstruction;
import pt.inescid.cllsj.compiler.ir.instructions.IRNewSession;
import pt.inescid.cllsj.compiler.ir.instructions.IRReturn;

public class IRFlowContinuation {
  private String label;
  private IRFlowLocation writer; // Where the continuation was set
  private Optional<IRFlowContinuation> override = Optional.empty();

  public IRFlowContinuation(String label, IRFlowLocation writer) {
    this.label = label;
    this.writer = writer;
  }

  public String getLabel() {
    return override.map(IRFlowContinuation::getLabel).orElse(label);
  }

  public IRFlowLocation getWriter() {
    return override.map(IRFlowContinuation::getWriter).orElse(writer);
  }

  public void setOverride(IRFlowContinuation override) {
    if (this.override.isPresent()) {
      this.override.get().setOverride(override);
    } else {
      this.override = Optional.of(override);
    }
  }

  public void replaceWritten(Optional<String> label) {
    if (override.isPresent()) {
      override.get().replaceWritten(label);
      return;
    }

    IRInstruction instruction = writer.getInstruction();
    if (instruction instanceof IRFlip) {
      IRFlip flip = (IRFlip) instruction;
      if (label.isEmpty()) {
        writer.replaceInstruction(new IRReturn(flip.getRecord()));
      } else {
        flip.setContLabel(label.get());
      }
    } else if (instruction instanceof IRNewSession) {
      IRNewSession newSession = (IRNewSession) instruction;
      newSession.setLabel(label);
    } else {
      throw new IllegalArgumentException(
          "Cannot replace continuation for instruction " + instruction + " at " + this);
    }
  }

  @Override
  public String toString() {
    return getLabel() + "@" + getWriter();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) return true;
    if (!(obj instanceof IRFlowContinuation)) return false;
    if (this.override.isPresent()) {
      return override.get().equals(obj);
    }
    IRFlowContinuation other = (IRFlowContinuation) obj;
    while (!other.override.isEmpty()) {
      other = other.override.get();
    }
    return this == other;
  }
}
