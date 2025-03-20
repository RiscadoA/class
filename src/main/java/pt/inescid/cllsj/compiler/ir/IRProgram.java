package pt.inescid.cllsj.compiler.ir;

import java.util.HashMap;
import java.util.Map;

public class IRProgram {
  private Map<String, IRProcess> processes;

  public IRProgram() {
    this.processes = new HashMap<>();
  }

  public void addProcess(String name, IRProcess process) {
    processes.put(name, process);
  }

  public Map<String, IRProcess> getProcesses() {
    return processes;
  }

  @Override
  public String toString() {
    String result = "";
    for (Map.Entry<String, IRProcess> entry : processes.entrySet()) {
      if (!result.isEmpty()) {
        result += "\n";
      }
      result += entry.getKey() + ":\n";
      result += entry.getValue().toString() + "\n";
    }
    return result;
  }
}
