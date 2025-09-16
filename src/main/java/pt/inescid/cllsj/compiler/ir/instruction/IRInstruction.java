package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.id.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRLocalDataId;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public abstract class IRInstruction {
  public abstract void accept(IRInstructionVisitor visitor);

  public abstract IRInstruction clone();

  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {
    replaceDataLocations(loc -> loc.replaceSessions(replacer));
  }

  public void replaceLocalData(Function<IRLocalDataId, IRLocalDataId> replacer) {
    replaceDataLocations(loc -> loc.replaceLocalData(replacer));
  }

  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {}

  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {}

  public void replaceProcesses(Function<IRProcessId, IRProcessId> replacer) {}

  public boolean usesSession(IRSessionId sessionId) {
    AtomicBoolean uses = new AtomicBoolean(false);
    replaceSessions(
        sid -> {
          if (sid.equals(sessionId)) {
            uses.set(true);
          }
          return sid;
        });
    return uses.get();
  }

  public boolean usesCodeLocation(IRCodeLocation location) {
    AtomicBoolean uses = new AtomicBoolean(false);
    replaceCodeLocations(
        loc -> {
          if (loc.equals(location)) {
            uses.set(true);
          }
          return loc;
        });
    return uses.get();
  }

  public boolean usesLocalData(IRLocalDataId localDataId) {
    AtomicBoolean uses = new AtomicBoolean(false);
    replaceLocalData(
        data -> {
          if (data.equals(localDataId)) {
            uses.set(true);
          }
          return data;
        });
    return uses.get();
  }

  public boolean usesProcess(IRProcessId processId) {
    AtomicBoolean uses = new AtomicBoolean(false);
    replaceProcesses(
        pid -> {
          if (pid.equals(processId)) {
            uses.set(true);
          }
          return pid;
        });
    return uses.get();
  }
}
