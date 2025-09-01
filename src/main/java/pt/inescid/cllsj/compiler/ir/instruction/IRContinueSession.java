package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRContinueSession extends IRInstruction {
  private IRSessionId sessionId;
  private IRCodeLocation continuation;

  public IRContinueSession(IRSessionId sessionId, IRCodeLocation continuation) {
    this.sessionId = sessionId;
    this.continuation = continuation;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public IRCodeLocation getContinuation() {
    return continuation;
  }

  @Override
  public IRInstruction clone() {
    return new IRContinueSession(sessionId, continuation);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    sessionId = replacer.apply(sessionId);
  }

  @Override
  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {
    continuation = replacer.apply(continuation);
  }

  @Override
  public String toString() {
    return "continueSession(" + sessionId + ", " + continuation + ")";
  }
}
