package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import pt.inescid.cllsj.compiler.ir.IRCodeLocation;
import pt.inescid.cllsj.compiler.ir.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.IRInstructionVisitor;
import pt.inescid.cllsj.compiler.ir.IRSessionId;

public abstract class IRInstruction {
  public abstract void accept(IRInstructionVisitor visitor);

  public abstract IRInstruction clone();

  public void replaceSessions(Function<IRSessionId, IRSessionId> replacer) {}

  public void replaceCodeLocations(Function<IRCodeLocation, IRCodeLocation> replacer) {}

  public void replaceDataLocations(Function<IRDataLocation, IRDataLocation> replacer) {}

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
}
