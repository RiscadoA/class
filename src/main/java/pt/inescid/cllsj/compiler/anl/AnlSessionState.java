package pt.inescid.cllsj.compiler.anl;

import java.util.Optional;
import pt.inescid.cllsj.compiler.ir.id.IRDataLocation;
import pt.inescid.cllsj.compiler.ir.id.IRSessionId;

public class AnlSessionState {
  public Optional<AnlFlowContinuation> cont = Optional.empty();
  public Optional<IRDataLocation> data = Optional.empty();
  public Optional<IRSessionId> remote = Optional.empty();

  public AnlSessionState clone() {
    AnlSessionState cloned = new AnlSessionState();
    cloned.cont = cont;
    cloned.data = data;
    cloned.remote = remote;
    return cloned;
  }

  public AnlSessionState merge(AnlSessionState other) {
    AnlSessionState clone = this.clone();
    if (this.cont.isEmpty() || other.cont.isEmpty()) {
      clone.cont = Optional.empty();
    } else {
      clone.cont = this.cont.get().merge(other.cont.get());
    }
    if (!this.data.equals(other.data)) {
      clone.data = Optional.empty();
    }
    if (!this.remote.equals(other.remote)) {
      clone.remote = Optional.empty();
    }
    return clone;
  }

  public void markAsUnknown(Analyzer analyzer, AnlFlowState state) {
    if (cont.isPresent()) {
      state.pushPendingContinuation(analyzer, cont.get());
      cont = Optional.empty();
    }
    if (data.isPresent()) {
      state.markDataAsUnknown(analyzer, data.get());
      data = Optional.empty();
    }
    if (remote.isPresent()) {
      state.session(remote.get()).remote = Optional.empty();
      if (remote.isPresent()) {
        state.session(remote.get()).markAsUnknown(analyzer, state);
        remote = Optional.empty();
      }
    }
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    b.append("cont=").append(cont.isPresent() ? cont.get().toString() : "?");
    b.append(" data=").append(data.isPresent() ? data.get().toString() : "?");
    b.append(" remote=").append(remote.isPresent() ? remote.get().toString() : "?");
    return b.toString();
  }
}
