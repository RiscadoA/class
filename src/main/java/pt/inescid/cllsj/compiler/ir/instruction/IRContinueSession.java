package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.Optional;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRContinueSession extends IRInstruction {
  private Optional<IRSessionId> sessionId;
  private Optional<IRDataLocation> sessionLocation;
  private IRCodeLocation continuation;

  public IRContinueSession(IRSessionId sessionId, IRCodeLocation continuation) {
    this.sessionId = Optional.of(sessionId);
    this.sessionLocation = Optional.empty();
    this.continuation = continuation;
  }

  public IRContinueSession(IRDataLocation sessionLocation, IRCodeLocation continuation) {
    this.sessionId = Optional.empty();
    this.sessionLocation = Optional.of(sessionLocation);
    this.continuation = continuation;
  }

  public boolean isById() {
    return sessionId.isPresent();
  }

  public IRSessionId getSessionId() {
    return sessionId.orElseThrow();
  }

  public IRDataLocation getSessionLocation() {
    return sessionLocation.orElseThrow();
  }

  public IRCodeLocation getContinuation() {
    return continuation;
  }

  @Override
  public IRInstruction clone() {
    if (sessionId.isPresent()) {
      return new IRContinueSession(sessionId.get(), continuation);
    } else {
      return new IRContinueSession(sessionLocation.get(), continuation);
    }
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    sessionId = sessionId.map(replacer);
  }

  @Override
  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {
    super.replaceDataLocations(replacer);
    sessionLocation = sessionLocation.map(replacer);
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    super.replaceCodeLocations(replacer);
    continuation = replacer.apply(continuation);
  }

  @Override
  public String toString() {
    if (isById()) {
      return "continueSession(" + sessionId.get() + ", " + continuation + ")";
    } else {
      return "continueSession(" + sessionLocation.get() + ", " + continuation + ")";
    }
  }
}
