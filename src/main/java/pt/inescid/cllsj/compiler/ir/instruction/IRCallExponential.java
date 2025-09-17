package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class IRCallExponential extends IRAccess {
  private IRSessionId sessionId;
  private IRLocalDataId localDataId;

  public IRCallExponential(
      IRDataLocation location, IRSessionId sessionId, IRLocalDataId localDataId) {
    super(location);
    this.sessionId = sessionId;
    this.localDataId = localDataId;
  }

  public IRSessionId getSessionId() {
    return sessionId;
  }

  public IRLocalDataId getLocalDataId() {
    return localDataId;
  }

  @Override
  public IRInstruction clone() {
    return new IRCallExponential(location, sessionId, localDataId);
  }

  @Override
  public void accept(IRInstructionVisitor visitor) {
    visitor.visit(this);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("callExponential(")
        .append(location)
        .append(", ")
        .append(sessionId)
        .append(", ")
        .append(localDataId)
        .append(")");
    return sb.toString();
  }

  @Override
  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    super.replaceSessions(replacer);
    sessionId = replacer.apply(sessionId);
  }

  @Override
  public void replaceLocalData(Function<IRLocalDataId, IRLocalDataId> replacer, boolean includeLocations) {
    super.replaceLocalData(replacer, includeLocations);
    localDataId = replacer.apply(localDataId);
  }
}
