package pt.inescid.cllsj.compiler.ir.instruction;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;
import pt.inescid.cllsj.compiler.ir.id.IRProcessId;

public class IRProgram {
  private Map<IRProcessId, IRProcess> processes = new HashMap<>();

  public void add(IRProcess process) {
    processes.put(process.getId(), process);
  }

  public IRProcess get(IRProcessId id) {
    return processes.get(id);
  }

  public Stream<IRProcess> stream() {
    return processes.values().stream();
  }

  public void removeIf(Function<IRProcess, Boolean> predicate) {
    processes.values().removeIf(p -> predicate.apply(p));
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    for (IRProcess proc : processes.values()) {
      if (!b.isEmpty()) {
        b.append("\n");
      }
      b.append(proc.toString());
    }
    return b.toString();
  }
}
