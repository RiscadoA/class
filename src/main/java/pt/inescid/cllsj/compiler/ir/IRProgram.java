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
    for (Map.Entry<String, IRProcess> process : processes.entrySet()) {
      if (!result.isEmpty()) {
        result += "\n";
      }
      result += process.getKey() + "[env]:\n";
      for (int i = 0; i < process.getValue().getTypeVariableCount(); i++) {
        result += "    type " + i + "\n";
      }
      for (int i = 0; i < process.getValue().getRecordCount(); i++) {
        result += "    record " + i + ": " + process.getValue().getRecordType(i).toString() + "\n";
      }
      for (int i = 0; i < process.getValue().getExponentialCount(); i++) {
        result +=
            "    exponential "
                + i
                + ": "
                + process.getValue().getExponentialType(i).toString()
                + "\n";
      }
      result += process.getKey() + ":\n";
      result += process.getValue().toString() + "\n";
    }
    return result;
  }
}
