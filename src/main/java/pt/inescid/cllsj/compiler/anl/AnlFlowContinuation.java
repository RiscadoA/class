package pt.inescid.cllsj.compiler.anl;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.instruction.IRContinueSession;
import pt.inescid.cllsj.compiler.ir.instruction.IRFinishSession;
import pt.inescid.cllsj.compiler.ir.instruction.IRInitializeSession;
import pt.inescid.cllsj.compiler.ir.instruction.IRInstruction;

public class AnlFlowContinuation {
  private IRCodeLocation location;
  private AnlFlowLocation writer; // Where the continuation was set
  private Optional<AnlFlowContinuation> override = Optional.empty();

  public AnlFlowContinuation(IRCodeLocation location, AnlFlowLocation writer) {
    this.location = location;
    this.writer = writer;
  }

  public IRCodeLocation getLocation() {
    return override.map(AnlFlowContinuation::getLocation).orElse(location);
  }

  public AnlFlowLocation getWriter() {
    return override.map(AnlFlowContinuation::getWriter).orElse(writer);
  }

  public void setOverride(AnlFlowContinuation override) {
    if (this.override.isPresent()) {
      this.override.get().setOverride(override);
    } else {
      this.override = Optional.of(override);
    }
  }

  public void replaceWritten(Optional<IRCodeLocation> location) {
    if (override.isPresent()) {
      override.get().replaceWritten(location);
      return;
    }

    IRInstruction instruction = writer.getInstruction();
    if (instruction instanceof IRContinueSession) {
      IRContinueSession cont = (IRContinueSession) instruction;
      if (location.isEmpty()) {
        writer.replaceInstruction(new IRFinishSession(cont.getSessionId(), false));
      } else {
        cont.replaceCodeLocations(r -> location.get());
      }
    } else if (instruction instanceof IRInitializeSession) {
      IRInitializeSession init = (IRInitializeSession) instruction;
      init.setContinuation(location);
    } else {
      throw new IllegalArgumentException(
          "Cannot replace continuation for instruction " + instruction + " at " + this);
    }
  }

  @Override
  public String toString() {
    return getLocation() + "@" + getWriter();
  }

  public Optional<AnlFlowContinuation> merge(AnlFlowContinuation other) {
    if (override.isPresent()) {
      return override.get().merge(other);
    }
    while (!other.override.isEmpty()) {
      other = other.override.get();
    }
    if (this == other) {
      return Optional.of(this);
    }

    if (location.equals(other.location) && writer.equals(other.writer)) {
      this.override = Optional.of(other);
      return Optional.of(this);
    } else {
      return Optional.empty();
    }
  }
}
