package pt.inescid.cllsj.compiler.anl;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;

public class AnlSessionState {
  public Optional<AnlFlowContinuation> cont = Optional.empty();
  public Optional<IRDataLocation> data = Optional.empty();

  public AnlSessionState clone() {
    AnlSessionState cloned = new AnlSessionState();
    cloned.cont = cont;
    cloned.data = data;
    return cloned;
  }

  public AnlSessionState merge(AnlSessionState other) {
    AnlSessionState clone = this.clone();
    if (!this.cont.equals(other.cont)) {
      clone.cont = Optional.empty();
    }
    if (!this.data.equals(other.data)) {
      clone.data = Optional.empty();
    }
    return clone;
  }
}
